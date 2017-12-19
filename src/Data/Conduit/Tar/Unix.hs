{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Conduit.Tar.Unix
    ( getFileInfo
    , restoreFile
    ) where

import           Conduit
import           Control.Monad                 (when)
import           Data.Bits
import qualified Data.ByteString.Char8         as S8
import           Data.Conduit.Tar.Types        (FileInfo (..), FileType (..))
import qualified System.Directory              as Dir
import qualified System.Posix.Files.ByteString as Posix
import qualified System.Posix.User             as Posix

getFileInfo :: S8.ByteString -> IO FileInfo
getFileInfo fp = do
    fs <- Posix.getSymbolicLinkStatus fp
    let uid = Posix.fileOwner fs
        gid = Posix.fileGroup fs
    uEntry <- Posix.getUserEntryForID uid
    gEntry <- Posix.getGroupEntryForID gid
    (fType, fSize) <-
        case () of
            () | Posix.isRegularFile fs     -> return (FTNormal, Posix.fileSize fs)
               | Posix.isSymbolicLink fs    -> do
                     ln <- Posix.readSymbolicLink fp
                     return (FTSymbolicLink ln, 0)
               | Posix.isCharacterDevice fs -> return (FTCharacterSpecial, 0)
               | Posix.isBlockDevice fs     -> return (FTBlockSpecial, 0)
               | Posix.isDirectory fs       -> return (FTDirectory, 0)
               | Posix.isNamedPipe fs       -> return (FTFifo, 0)
               | otherwise                  -> error $ "Unsupported file type: " ++ S8.unpack fp
    return FileInfo
        { filePath      = fp
        , fileUserId    = uid
        , fileUserName  = S8.pack $ Posix.userName uEntry
        , fileGroupId   = gid
        , fileGroupName = S8.pack $ Posix.groupName gEntry
        , fileMode      = Posix.fileMode fs .&. 0o7777
        , fileSize      = fSize
        , fileType      = fType
        , fileModTime   = Posix.modificationTime fs
        }

-- | Restore files onto the file system. Produces actions that will set the modification time on the
-- directories, which can be executed after the pipeline has finished and all files have been
-- written to disk.
restoreFile :: (MonadResource m) =>
               FileInfo -> ConduitM S8.ByteString (IO ()) m ()
restoreFile FileInfo {..} = do
    let filePath' = S8.unpack filePath
    case fileType of
        FTDirectory -> do
            liftIO $ Dir.createDirectoryIfMissing False filePath'
            yield $
                (Dir.doesDirectoryExist filePath' >>=
                 (`when` Posix.setFileTimes filePath fileModTime fileModTime))
        FTSymbolicLink link ->
            liftIO $ do
                exist <- Posix.fileExist filePath
                when exist $ Dir.removeFile filePath'
                Posix.createSymbolicLink link filePath
        FTNormal -> sinkFile filePath'
        ty -> error $ "Unsupported tar entry type: " ++ show ty
    liftIO $ do
        Posix.setFileTimes filePath fileModTime fileModTime
        Posix.setSymbolicLinkOwnerAndGroup filePath fileUserId fileGroupId
        Posix.setFileMode filePath fileMode
