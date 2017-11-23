-- | Module contains all the types necessary for tarball processing.
module Data.Conduit.Tar.Types
    ( Header(..)
    , TarChunk(..)
    , TarException(..)
    , TarCreateException(..)
    , FileType(..)
    , FileInfo(..)
    , FileOffset
    , ByteCount
    , UserID
    , GroupID
    , DeviceID
    , EpochTime
    ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import System.Posix.Types
import Data.Typeable
import Data.Word

data FileType
    = FTNormal
    | FTHardLink
    | FTSymbolicLink !FilePath
    | FTCharacterSpecial
    | FTBlockSpecial
    | FTDirectory
    | FTFifo
    | FTOther !Word8
    deriving (Show, Eq)


data FileInfo = FileInfo
    { filePath      :: !FilePath -- ^ File path.
    , fileUserId    :: !UserID  -- ^ Unix user id.
    , fileUserName  :: !String  -- ^ Unix user name.
    , fileGroupId   :: !GroupID -- ^ Unix group id.
    , fileGroupName :: !String  -- ^ Unix group name.
    , fileMode      :: !FileMode -- ^ Unix file permissions
    , fileSize      :: !FileOffset -- ^ File size
    , fileType      :: !FileType  -- ^ File type. `FTNormal`, `FTSymbolicLink`
                                  -- and `FTDirectory` are the only ones
                                  -- supported for now
    , fileModTime   :: !EpochTime -- ^ File modification timestamp
    } deriving Show


data Header = Header
    { headerOffset         :: !FileOffset
    , headerPayloadOffset  :: !FileOffset
    , headerFileNameSuffix :: !ShortByteString
    , headerFileMode       :: !CMode
    , headerOwnerId        :: !UserID
    , headerGroupId        :: !GroupID
    , headerPayloadSize    :: !FileOffset
    , headerTime           :: !EpochTime
    , headerLinkIndicator  :: !Word8
    , headerLinkName       :: !ShortByteString
    , headerMagicVersion   :: !ShortByteString
    , headerOwnerName      :: !ShortByteString
    , headerGroupName      :: !ShortByteString
    , headerDeviceMajor    :: !DeviceID
    , headerDeviceMinor    :: !DeviceID
    , headerFileNamePrefix :: !ShortByteString
    }
    deriving Show



data TarChunk
    = ChunkHeader Header
    | ChunkPayload !FileOffset !ByteString
    | ChunkException TarException
    deriving Show

-- | This the the exception type that is used in this module.
--
-- More constructors are susceptible to be added without bumping the major
-- version of this module.
data TarException
    = NoMoreHeaders
    | UnexpectedPayload !FileOffset
    | IncompleteHeader  !FileOffset
    | IncompletePayload !FileOffset !ByteCount
    | ShortTrailer      !FileOffset
    | BadTrailer        !FileOffset
    | InvalidHeader     !FileOffset
    | BadChecksum       !FileOffset
    | FileTypeError     !FileOffset !Char !String
    deriving (Show, Typeable)
instance Exception TarException


data TarCreateException
    = FileNameTooLong   !FileInfo
    | TarCreationError  !String
    deriving (Show, Typeable)
instance Exception TarCreateException
