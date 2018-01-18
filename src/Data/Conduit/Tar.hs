{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-| This module is about stream-processing tar archives. It is currently
not very well tested. See the documentation of 'withEntries' for an usage sample.
-}
module Data.Conduit.Tar
    ( -- * Basic functions
      untar
    , withEntry
    , withEntries
      -- * Helper functions
    , headerFileType
    , headerFilePath
      -- * Types
    , Header (..)
    , TarChunk (..)
    , TarException (..)
    , Offset
    , Size
    , FileType (..)
    ) where

import Conduit
import Control.Exception (Exception, assert)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import qualified Data.ByteString        as S
import qualified Data.ByteString.Char8  as S8
import qualified Data.ByteString.Unsafe as BU
import System.Posix.Types (CMode)
import Data.Word (Word8)
import Data.Int (Int64)
import Data.ByteString.Short (ShortByteString, toShort, fromShort)
import Data.Monoid ((<>))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif

data Header = Header
    { headerOffset         :: !Offset
    , headerPayloadOffset  :: !Offset
    , headerFileNameSuffix :: !ShortByteString
    , headerFileMode       :: !CMode
    , headerOwnerId        :: !Int
    , headerGroupId        :: !Int
    , headerPayloadSize    :: !Size
    , headerTime           :: !Int64
    , headerLinkIndicator  :: !Word8
    , headerOwnerName      :: !ShortByteString
    , headerGroupName      :: !ShortByteString
    , headerDeviceMajor    :: !Int
    , headerDeviceMinor    :: !Int
    , headerFileNamePrefix :: !ShortByteString
    }
    deriving Show

headerFilePath :: Header -> FilePath
headerFilePath h = S8.unpack $ fromShort
                 $ headerFileNamePrefix h <> headerFileNameSuffix h

data FileType
    = FTNormal
    | FTHardLink
    | FTSymbolicLink
    | FTCharacterSpecial
    | FTBlockSpecial
    | FTDirectory
    | FTFifo
    | FTOther !Word8
    deriving (Show, Eq)

headerFileType :: Header -> FileType
headerFileType h =
    case headerLinkIndicator h of
        0  -> FTNormal
        48 -> FTNormal
        49 -> FTHardLink
        50 -> FTSymbolicLink
        51 -> FTCharacterSpecial
        52 -> FTBlockSpecial
        53 -> FTDirectory
        54 -> FTFifo
        x  -> FTOther x

type Offset = Int
type Size = Int

data TarChunk
    = ChunkHeader Header
    | ChunkPayload !Offset !ByteString
    | ChunkException TarException
    deriving Show

-- | This the the exception type that is used in this module.
--
-- More constructors are susceptible to be added without bumping the major
-- version of this module.
data TarException
    = NoMoreHeaders
    | UnexpectedPayload !Offset
    | IncompleteHeader  !Offset
    | IncompletePayload !Offset !Size
    | ShortTrailer      !Offset
    | BadTrailer        !Offset
    | InvalidHeader     !Offset
    | BadChecksum       !Offset
    deriving (Show, Typeable)
instance Exception TarException

parseHeader :: Offset -> ByteString -> Either TarException Header
parseHeader offset bs = assert (S.length bs == 512) $ do
    let checksumBytes = S.take 8 $ S.drop 148 bs
        expectedChecksum = parseOctal checksumBytes
        actualChecksum = bsum bs - bsum checksumBytes + 8 * space
    unless (actualChecksum == expectedChecksum) (Left (BadChecksum offset))
    return Header
        { headerOffset         = offset
        , headerPayloadOffset  = offset + 512
        , headerFileNameSuffix = short headerFileNameSuffix'
        , headerFileMode       = octal headerFileMode'
        , headerOwnerId        = octal headerOwnerId'
        , headerGroupId        = octal headerGroupId'
        , headerPayloadSize    = octal headerPayloadSize'
        , headerTime           = octal headerTime'
        , headerLinkIndicator  = BU.unsafeIndex bs 156
        , headerOwnerName      = short headerOwnerName'
        , headerGroupName      = short headerGroupName'
        , headerDeviceMajor    = octal headerDeviceMajor'
        , headerDeviceMinor    = octal headerDeviceMinor'
        , headerFileNamePrefix = short headerFileNamePrefix'
        }
  where
    bsum :: ByteString -> Int
    bsum = S.foldl' (\c n -> c + fromIntegral n) 0

    rest1 = bs
    (headerFileNameSuffix', rest2) = S.splitAt 100 rest1
    (headerFileMode',       rest3) = S.splitAt 8   rest2
    (headerOwnerId',        rest4) = S.splitAt 8   rest3
    (headerGroupId',        rest5) = S.splitAt 8   rest4
    (headerPayloadSize',    rest6) = S.splitAt 12  rest5
    headerTime'                    = S.take    12  rest6


    (headerOwnerName',   bs1) = S.splitAt 32  $ S.drop 265 bs
    (headerGroupName',   bs2) = S.splitAt 32  bs1
    (headerDeviceMajor', bs3) = S.splitAt 8   bs2
    (headerDeviceMinor', bs4) = S.splitAt 8   bs3
    headerFileNamePrefix'     = S.take    155 bs4

    short = toShort . S.takeWhile (/= 0)
    octal :: Integral i => ByteString -> i
    octal = parseOctal

    parseOctal :: Integral i => ByteString -> i
    parseOctal = S.foldl' (\t c -> t * 8 + fromIntegral (c - zero)) 0
               . S.takeWhile (\c -> zero <= c && c <= seven)
               . S.dropWhile (== space)

    space :: Integral i => i
    space = 0x20
    zero = 48
    seven = 55

untar :: Monad m => ConduitM ByteString TarChunk m ()
untar =
    loop 0
  where
    loop !offset = assert (offset `mod` 512 == 0) $ do
        bs <- takeCE 512 .| foldC
        case S.length bs of
            0 -> return ()
            512 | S.all (== 0) bs -> do
                let offset' = offset + 512
                bs' <- takeCE 512 .| foldC
                case () of
                    ()
                        | S.length bs' /= 512 -> do
                            leftover bs'
                            yield $ ChunkException $ ShortTrailer offset'
                        | S.all (== 0) bs' -> return ()
                        | otherwise -> do
                            leftover bs'
                            yield $ ChunkException $ BadTrailer offset'
            512 ->
                case parseHeader offset bs of
                    Left e -> do
                        leftover bs
                        yield $ ChunkException e
                    Right h -> do
                        yield $ ChunkHeader h
                        offset' <- payloads (offset + 512) $ headerPayloadSize h
                        let expectedOffset = offset + 512 + headerPayloadSize h +
                                (case (512 - (headerPayloadSize h `mod` 512)) of
                                    512 -> 0
                                    x -> x)
                        assert (offset' == expectedOffset) (loop offset')
            _ -> do
                leftover bs
                yield $ ChunkException $ IncompleteHeader offset

    payloads !offset 0 = do
        let padding =
                case offset `mod` 512 of
                    0 -> 0
                    x -> 512 - x
        takeCE padding .| sinkNull
        return $! offset + padding
    payloads !offset !size = do
        mbs <- await
        case mbs of
            Nothing -> do
                yield $ ChunkException $ IncompletePayload offset size
                return offset
            Just bs -> do
                let (x, y) = S.splitAt size bs
                yield $ ChunkPayload offset x
                let size' = size - S.length x
                    offset' = offset + S.length x
                unless (S.null y) (leftover y)
                payloads offset' size'

-- | Process a single tar entry. See 'withEntries' for more details.
withEntry :: MonadThrow m
          => (Header -> ConduitM ByteString o m r)
          -> ConduitM TarChunk o m r
withEntry inner = do
    mc <- await
    case mc of
        Nothing -> throwM NoMoreHeaders
        Just (ChunkHeader h) -> payloads .| (inner h <* sinkNull)
        Just x@(ChunkPayload offset _bs) -> do
            leftover x
            throwM $ UnexpectedPayload offset
        Just (ChunkException e) -> throwM e
  where
    payloads = do
        mx <- await
        case mx of
            Just (ChunkPayload _ bs) -> yield bs >> payloads
            Just x@ChunkHeader{} -> leftover x
            Just (ChunkException e) -> throwM e
            Nothing -> return ()

{-| This function handles each entry of the tar archive according to the
behaviour of the function passed as first argument.

Here is a full example function, that reads a compressed tar archive and for each entry that is a simple file, it prints its file path and SHA256 digest. Note that this function can throw exceptions!

> import qualified Crypto.Hash.Conduit as CH
> import qualified Data.Conduit.Tar    as CT
> 
> import Conduit
> import Crypto.Hash (Digest, SHA256)
> import Control.Monad (when)
> import Data.Conduit.Zlib (ungzip)
> import Data.ByteString (ByteString)
> 
> filedigests :: FilePath -> IO ()
> filedigests fp = runConduitRes (  sourceFileBS fp          -- read the raw file
>                                .| ungzip                   -- gunzip
>                                .| CT.untar                 -- decode the tar archive
>                                .| CT.withEntries hashentry -- process each file
>                                .| printC                   -- print the results
>                                )
>     where
>         hashentry :: Monad m => CT.Header -> Conduit ByteString m (FilePath, Digest SHA256)
>         hashentry hdr = when (CT.headerFileType hdr == CT.FTNormal) $ do
>             hash <- CH.sinkHash
>             yield (CT.headerFilePath hdr, hash)

The @hashentry@ function handles a single entry, based on its first 'Header' argument.
In this example, a 'Consumer' is used to process the whole entry.

Note that the benefits of stream processing are easily lost when working with a 'Consumer'. For example, the following implementation would have used an unbounded amount of memory:

>         hashentry hdr = when (CT.headerFileType hdr == CT.FTNormal) $ do
>             content <- mconcat <$> sinkList
>             yield (CT.headerFilePath hdr, hash content)
-}
withEntries :: MonadThrow m
            => (Header -> ConduitM ByteString o m ())
            -> ConduitM TarChunk o m ()
withEntries = peekForever . withEntry
