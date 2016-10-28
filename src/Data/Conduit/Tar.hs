{-# LANGUAGE BangPatterns #-}
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

data TarException
    = NoMoreHeaders
    | UnexpectedPayload !Offset
    | IncompleteHeader  !Offset
    | IncompletePayload !Offset !Size
    | ShortTrailer      !Offset
    | BadTrailer        !Offset
    | InvalidHeader     !Offset
    deriving (Show, Typeable)
instance Exception TarException

parseHeader :: Offset -> ByteString -> Either TarException Header
parseHeader offset bs = assert (S.length bs == 512) $ do
    let checksum = S.take 8 $ S.drop 148 bs
    return Header
        { headerOffset         = offset
        , headerPayloadOffset  = offset + 512
        , headerFileNameSuffix = getShort 0 100
        , headerFileMode       = getOctal 100 8
        , headerOwnerId        = getOctal 108 8
        , headerGroupId        = getOctal 116 8
        , headerPayloadSize    = getOctal 124 12
        , headerTime           = getOctal 136 12
        , headerLinkIndicator  = BU.unsafeIndex bs 156
        , headerOwnerName      = getShort 265 32
        , headerGroupName      = getShort 297 32
        , headerDeviceMajor    = getOctal 329 8
        , headerDeviceMinor    = getOctal 337 8
        , headerFileNamePrefix = getShort 345 155
        }
  where
    getShort off len = toShort $ S.takeWhile (/= 0) $ S.take len $ S.drop off bs

    getOctal off len = parseOctal $ S.take len $ S.drop off bs

    parseOctal :: Integral i => ByteString -> i
    parseOctal = S.foldl' (\t c -> t * 8 + fromIntegral (c - zero)) 0
               . S.takeWhile (\c -> zero <= c && c <= seven)

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
