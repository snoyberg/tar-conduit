{-# LANGUAGE CPP #-}
module Data.Conduit.Tar.Unix
    ( getFileInfo
    , Posix.readSymbolicLink
    , Dir.doesDirectoryExist
    ) where

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
    fType <-
        case () of
            () | Posix.isRegularFile fs     -> return FTNormal
               | Posix.isSymbolicLink fs    -> return FTSymbolicLink
               | Posix.isCharacterDevice fs -> return FTCharacterSpecial
               | Posix.isBlockDevice fs     -> return FTBlockSpecial
               | Posix.isDirectory fs       -> return FTDirectory
               | Posix.isNamedPipe fs       -> return FTFifo
               | otherwise                  -> error $ "Unsupported file type: " ++ fp
    return FileInfo
        { filePath      = fp
        , fileUserID    = uid
        , fileUserName  = Posix.userName uEntry
        , fileGroupID   = gid
        , fileGroupName = Posix.groupName gEntry
        , fileMode      = Posix.fileMode fs .&. 0o7777
        , fileSize      = Posix.fileSize fs
        , fileType      = fType
        , fileModTime   = Posix.modificationTime fs
        }

