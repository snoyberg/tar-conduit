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
import           Data.Conduit.Tar.Types   (FileInfo (..), FileType (..))
import           Data.Time.Clock.POSIX
import           Foreign.C.Types          (CTime (..))
import qualified System.Directory         as Dir
import qualified System.PosixCompat.Files as Posix

getFileInfo :: S8.ByteString -> IO FileInfo
getFileInfo fp = do
    let fp' = S8.unpack fp
    fs <- Posix.getSymbolicLinkStatus fp'
    let uid = Posix.fileOwner fs
        gid = Posix.fileGroup fs
    (fType, fSize) <-
        case () of
            () | Posix.isRegularFile fs     -> return (FTNormal, Posix.fileSize fs)
               | Posix.isDirectory fs       -> return (FTDirectory, 0)
               | otherwise                  -> error $ "Unsupported file type: " ++ fp'
    return FileInfo
        { filePath      = fp
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
    let filePath' = S8.unpack filePath
        CTime modTimeEpoch = fileModTime
        modTime = posixSecondsToUTCTime (fromIntegral modTimeEpoch)
    case fileType of
        FTDirectory -> do
            liftIO $ Dir.createDirectoryIfMissing False filePath'
            yield $
                (Dir.doesDirectoryExist filePath' >>=
                 (`when` Dir.setModificationTime filePath' modTime))
        FTNormal -> sinkFile filePath'
        ty -> error $ "Unsupported tar entry type: " ++ show ty
    liftIO $ do
        Dir.setModificationTime filePath' modTime
        Posix.setSymbolicLinkOwnerAndGroup filePath' fileUserId fileGroupId
        Posix.setFileMode filePath' fileMode

