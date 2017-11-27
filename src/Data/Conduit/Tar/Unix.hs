{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Data.Conduit.Tar.Unix
    ( getFileInfo
    , restoreFile
    ) where

import Conduit
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8  as S8
import qualified System.Directory as Dir
import qualified System.Posix.Files as Posix
import qualified System.Posix.User as Posix
import System.Posix.Types
import System.IO.Error
import Data.Bits
import Data.Conduit.Tar.Types (FileInfo(..), FileType(..))


getFileInfo :: FilePath -> IO FileInfo
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
               | otherwise                  -> error $ "Unsupported file type: " ++ fp
    return FileInfo
        { filePath      = S8.pack fp
        , fileUserId    = uid
        , fileUserName  = S8.pack $ Posix.userName uEntry
        , fileGroupId   = gid
        , fileGroupName = S8.pack $ Posix.groupName gEntry
        , fileMode      = Posix.fileMode fs .&. 0o7777
        , fileSize      = fSize
        , fileType      = fType
        , fileModTime   = Posix.modificationTime fs
        }


restoreFile :: (MonadResource m) =>
               FileInfo -> ConduitM ByteString o m ()
restoreFile FileInfo {..} = do
    let filePath' = S8.unpack filePath
    case fileType of
        FTDirectory -> liftIO $ Dir.createDirectoryIfMissing False filePath'
        FTSymbolicLink link ->
            liftIO $ do
                exist <- Posix.fileExist filePath'
                when exist $ Dir.removeFile filePath'
                Posix.createSymbolicLink link filePath'
        FTNormal -> do
            sinkFile filePath'
        ty -> error $ "Unsupported tar entry type: " ++ show ty
    liftIO $ do
        Posix.setOwnerAndGroup filePath' fileUserId fileGroupId
        Posix.setFileMode filePath' fileMode
        Posix.setFileTimes filePath' fileModTime fileModTime
