{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Tar.Unix
    ( getFileInfo
    , restoreFile
    ) where

import           Conduit
import           Control.Exception.Safe
import           Control.Monad                 (when, unless)
import           Data.Bits
import qualified Data.ByteString.Char8         as S8
import           Data.Either
import           Data.Conduit.Tar.Types
import           Foreign.C.Types               (CTime (..))
import qualified System.Directory              as Dir
import qualified System.Posix.Files            as Posix
import qualified System.Posix.User             as Posix


getFileInfo :: FilePath -> IO FileInfo
getFileInfo fpStr = do
    let fp = encodeFilePath fpStr
    fs <- Posix.getSymbolicLinkStatus fpStr
    let uid = Posix.fileOwner fs
        gid = Posix.fileGroup fs
    -- Allow for username/group retrieval failure, especially useful for non-tty environment.
    -- Workaround for: https://ghc.haskell.org/trac/ghc/ticket/1487
    -- Moreover, names are non-critical as they are not used during unarchival process
    euEntry :: Either IOException Posix.UserEntry <- try $ Posix.getUserEntryForID uid
    egEntry :: Either IOException Posix.GroupEntry <- try $ Posix.getGroupEntryForID gid
    (fType, fSize) <-
        case () of
            () | Posix.isRegularFile fs     -> return (FTNormal, Posix.fileSize fs)
               | Posix.isSymbolicLink fs    -> do
                     ln <- Posix.readSymbolicLink fpStr
                     return (FTSymbolicLink (encodeFilePath ln), 0)
               | Posix.isCharacterDevice fs -> return (FTCharacterSpecial, 0)
               | Posix.isBlockDevice fs     -> return (FTBlockSpecial, 0)
               | Posix.isDirectory fs       -> return (FTDirectory, 0)
               | Posix.isNamedPipe fs       -> return (FTFifo, 0)
               | otherwise                  -> error $ "Unsupported file type: " ++ S8.unpack fp
    return $! FileInfo
        { filePath      = fp
        , fileUserId    = uid
        , fileUserName  = either (const "") (S8.pack . Posix.userName) euEntry
        , fileGroupId   = gid
        , fileGroupName = either (const "") (S8.pack . Posix.groupName) egEntry
        , fileMode      = Posix.fileMode fs .&. 0o7777
        , fileSize      = fSize
        , fileType      = fType
        , fileModTime   = Posix.modificationTime fs
        }


-- | Restore files onto the file system. Produces actions that will set the modification time on the
-- directories, which can be executed after the pipeline has finished and all files have been
-- written to disk.
restoreFile :: (MonadResource m) =>
               FileInfo -> ConduitM S8.ByteString (IO (FileInfo, [SomeException])) m ()
restoreFile fi@FileInfo {..} = do
    let fpStr = decodeFilePath filePath
        restorePermissions = do
            eExc1 <- tryAny $ Posix.setOwnerAndGroup fpStr fileUserId fileGroupId
            eExc2 <- tryAny $ Posix.setFileMode fpStr fileMode
            return $! fst $ partitionEithers [eExc1, eExc2]
    case fileType of
        FTDirectory -> do
            excs <- liftIO $ do
                Dir.createDirectoryIfMissing False fpStr
                restorePermissions
            yield $ do
                eExc <- tryAny (Dir.doesDirectoryExist fpStr >>=
                                (`when` Posix.setFileTimes fpStr fileModTime fileModTime))
                return (fi, either ((excs ++) . pure) (const excs) eExc)
        FTSymbolicLink link -> do
            excs <- liftIO $ do
                -- Try to unlink any existing file/symlink
                eExc1 <- tryAny $ Posix.removeLink fpStr
                Posix.createSymbolicLink (decodeFilePath link) fpStr
                eExc2 <- tryAny $ Posix.setSymbolicLinkOwnerAndGroup fpStr fileUserId fileGroupId
                -- Try best effort in setting symbolic link modification time.
#if MIN_VERSION_unix(2,7,0)
                let CTime epochInt32 = fileModTime
                    unixModTime = fromInteger (fromIntegral epochInt32)
                eExc3 <- tryAny $ Posix.setSymbolicLinkTimesHiRes fpStr unixModTime unixModTime
                return $ fst $ partitionEithers [eExc1, eExc2, eExc3]
#else
                return $ fst $ partitionEithers [eExc1, eExc2]
#endif
            unless (null excs) $ yield (return (fi, excs))
        FTNormal -> do
            sinkFile fpStr
            excs <- liftIO $ do
                excs <- restorePermissions
                eExc <- tryAny $ Posix.setFileTimes fpStr fileModTime fileModTime
                return (either ((excs ++) . pure) (const excs) eExc)
            unless (null excs) $ yield (return (fi, excs))
        ty -> yield $ return (fi, [toException $ UnsupportedType ty])

