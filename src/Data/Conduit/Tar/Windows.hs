{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Conduit.Tar.Windows
    ( getFileInfo
    , restoreFile
    ) where

import           Conduit
import           Control.Monad            (when)
import           Data.Bits
import qualified Data.ByteString.Char8    as S8
import           Data.Conduit.Tar.Types
import           Data.Time.Clock.POSIX
import           Foreign.C.Types          (CTime (..))
import qualified System.Directory         as Dir
import qualified System.PosixCompat.Files as Posix


getFileInfo :: FilePath -> IO FileInfo
getFileInfo fp = do
    fs <- Posix.getSymbolicLinkStatus fp
    let uid = fromIntegral $ Posix.fileOwner fs
        gid = fromIntegral $ Posix.fileGroup fs
    (fType, fSize) <-
        case () of
            () | Posix.isRegularFile fs     -> return (FTNormal, Posix.fileSize fs)
               | Posix.isDirectory fs       -> return (FTDirectory, 0)
               | otherwise                  -> error $ "Unsupported file type: " ++ fp
    return FileInfo
        { filePath      = encodeFilePath fp
        , fileUserId    = uid
        , fileUserName  = ""
        , fileGroupId   = gid
        , fileGroupName = ""
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
    let fpStr = decodeFilePath filePath
        CTime modTimeEpoch = fileModTime
        modTime = posixSecondsToUTCTime (fromIntegral modTimeEpoch)
    case fileType of
        FTDirectory -> do
            liftIO $ Dir.createDirectoryIfMissing False fpStr
            yield $
                (Dir.doesDirectoryExist fpStr >>=
                 (`when` Dir.setModificationTime fpStr modTime))
        FTNormal -> sinkFile fpStr
        ty -> error $ "Unsupported tar entry type: " ++ show ty
    liftIO $ do
        Dir.setModificationTime fpStr modTime
        Posix.setFileMode fpStr fileMode

