{-# LANGUAGE CPP #-}
module Data.Conduit.Tar.Windows
    ( FileInfo
    , getFileInfo
    , filePath
    , fileUserID
    , fileUserName
    , fileGroupID
    , fileGroupName
    , fileMode
    , fileSize
    , isDirectory
    , isRegularFile
    , isSymbolicLink
    , modificationTime
    , Posix.readSymbolicLink

    , Dir.doesDirectoryExist
    ) where

import qualified System.Directory as Dir
import qualified System.Posix.Files as Posix
import qualified System.Posix.User as Posix
import System.Posix.Types
import Data.Bits

data FileInfo = FileInfo
    { filePath         :: !FilePath
    , fileUserID       :: !UserID
    , fileUserName     :: !String
    , fileGroupID      :: !GroupID
    , fileGroupName    :: !String
    , fileMode         :: !FileMode
    , fileSize         :: !FileOffset
    , isDirectory      :: !Bool
    , isRegularFile    :: !Bool
    , isSymbolicLink   :: !Bool
    , modificationTime :: !EpochTime
    } deriving Show

#if MIN_VERSION_directory(1,3,1)
import System.Directory (getSymbolicLinkTarget, pathIsSymbolicLink)

getFileInfo :: FilePath -> IO FileInfo
getFileInfo = undefined
    -- fp = do
    -- fs <- Posix.getSymbolicLinkStatus fp
    -- let uid = Posix.fileOwner fs
    --     gid = Posix.fileGroup fs
    -- uEntry <- Posix.getUserEntryForID uid
    -- gEntry <- Posix.getGroupEntryForID gid
    -- return FileInfo
    --     { filePath         = fp
    --     , fileUserID       = uid
    --     , fileUserName     = Posix.userName uEntry
    --     , fileGroupID      = gid
    --     , fileGroupName    = Posix.groupName gEntry
    --     , fileMode         = Posix.fileMode fs .&. 0o7777
    --     , fileSize         = Posix.fileSize fs
    --     , modificationTime = Posix.modificationTime fs
    --     , isSymbolicLink   = Posix.isSymbolicLink fs
    --     , isDirectory      = Posix.isDirectory fs
    --     , isRegularFile    = Posix.isRegularFile fs
    --     }

readSymbolicLink = getSymbolicLinkTarget

#else
getFileInfo _ =
    error $ "Impossible happened: tar-conduit dependency on directory < 1.3.1 is not supported"
readSymbolicLink = undefined
pathIsSymbolicLink = undefined
getSymbolicLinkTarget = undefined
#endif
