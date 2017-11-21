{-# LANGUAGE RecordWildCards #-}
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
    , module Data.Conduit.Tar.Types
    ) where

import Conduit
import Control.Exception (Exception, assert)
import Control.Monad (unless, when, void)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Foldable (foldr')
import Data.List (sort, nub)
import Data.Bits ((.&.))
import qualified Data.ByteString        as S
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Char8  as S8
import qualified Data.ByteString.Short  as SS
import qualified Data.ByteString.Lazy as SL
import qualified Data.ByteString.Unsafe as BU
import Foreign.C.Types (CTime(..))
import System.Posix.Types (CMode)
import System.FilePath
import Data.Word (Word8)
import Data.Int (Int64)
import Data.ByteString.Short (ShortByteString, toShort, fromShort)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#endif

import Data.Conduit.Tar.Types
#ifdef WINDOWS
import Data.Conduit.Tar.Windows
#else
import Data.Conduit.Tar.Unix
#endif



headerFilePath :: Header -> FilePath
headerFilePath h = S8.unpack $ fromShort
                 $ headerFileNamePrefix h <> headerFileNameSuffix h


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

parseHeader :: FileOffset -> ByteString -> Either TarException Header
parseHeader offset bs = assert (S.length bs == 512) $ do
    let checksumBytes = S.take 8 $ S.drop 148 bs
        expectedChecksum = parseOctal checksumBytes
        actualChecksum = bsum bs - bsum checksumBytes + 8 * space
    unless (actualChecksum == expectedChecksum) (Left (BadChecksum offset))
    return Header
        { headerOffset         = offset
        , headerPayloadOffset  = offset + 512
        , headerFileNameSuffix = getShort 0 100
        , headerFileMode       = getOctal 100 8
        , headerOwnerId        = getOctal 108 8
        , headerGroupId        = getOctal 116 8
        , headerPayloadSize    = getOctal 124 12
        , headerTime           = CTime $ getOctal 136 12
        , headerLinkIndicator  = BU.unsafeIndex bs 156
        , headerLinkName       = getShort 157 100
        , headerOwnerName      = getShort 265 32
        , headerGroupName      = getShort 297 32
        , headerDeviceMajor    = getOctal 329 8
        , headerDeviceMinor    = getOctal 337 8
        , headerFileNamePrefix = getShort 345 155
        }
  where
    bsum :: ByteString -> Int
    bsum = S.foldl' (\c n -> c + fromIntegral n) 0

    getShort off len = toShort $ S.takeWhile (/= 0) $ S.take len $ S.drop off bs

    getOctal off len = parseOctal $ S.take len $ S.drop off bs

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
                                (case 512 - (headerPayloadSize h `mod` 512) of
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
                    x -> 512 - fromIntegral x
        takeCE padding .| sinkNull
        return $! offset + fromIntegral padding
    payloads !offset !size = do
        mbs <- await
        case mbs of
            Nothing -> do
                yield $ ChunkException $ IncompletePayload offset $ fromIntegral size
                return offset
            Just bs -> do
                let (x, y) =
                        S.splitAt (fromIntegral (size `mod` fromIntegral defaultChunkSize)) bs
                yield $ ChunkPayload offset x
                let size' = size - fromIntegral (S.length x)
                    offset' = offset + fromIntegral (S.length x)
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



--------------------------------------------------------------------------------
-- Create a tar file -----------------------------------------------------------
--------------------------------------------------------------------------------

blockSize :: FileOffset
blockSize = 512


headerFromFileInfo :: (MonadThrow m, MonadIO m) =>
                      FileOffset -- ^ Starting offset within the tarball. Must
                      -- be multiple of 512, otherwise error.
                   -> FileInfo -- ^ File info.
                   -> m Header
headerFromFileInfo offset fi = do
    unless (offset `mod` 512 == 0) $
        throwM $ TarCreationError $ "Offset must always be a multiple of 512"
    let (prefix, suffix) = splitPathAt 100 $ filePath fi
    (payloadSize, linkIndicator) <-
        case fileType fi of
          FTNormal       -> return (fileSize fi, 48)
          FTSymbolicLink -> return (0, 50)
          FTDirectory    -> return (0, 53)
          fty            -> throwM $ TarCreationError $ "Unsupported file type: " ++ show fty
    return Header
          { headerOffset = offset
          , headerPayloadOffset = offset + 512
          , headerFileNameSuffix = suffix
          , headerFileMode = fileMode fi
          , headerOwnerId = fileUserID fi
          , headerGroupId = fileGroupID fi
          , headerPayloadSize = payloadSize
          , headerTime = fileModTime fi
          , headerLinkIndicator = linkIndicator
          , headerLinkName = SS.empty
          , headerOwnerName = SS.toShort (S8.pack (fileUserName fi))
          , headerGroupName = SS.toShort (S8.pack (fileGroupName fi))
          , headerDeviceMajor = 0
          , headerDeviceMinor = 0
          , headerFileNamePrefix = prefix
          }


-- | Split a file path at the @n@ mark from the end, while still keeping the
-- split as a valid path, i.e split at a path separator only.
splitPathAt :: Int -> FilePath -> (ShortByteString, ShortByteString)
splitPathAt n fp
    | length fp < n = (SS.empty, SS.toShort (S8.pack fp))
    | otherwise =
        let sfp = splitPath fp
            toShortPath = SS.toShort . S8.pack . joinPath
            sepWith p (tlen, prefix, suffix) =
                case tlen + length p of
                    tlen'
                        | tlen' <= n -> (tlen', prefix, p : suffix)
                    tlen' -> (tlen', p : prefix, suffix)
            (_, prefix, suffix) = foldr' sepWith (0, [], []) sfp
        in (toShortPath prefix, toShortPath suffix)


packHeader :: MonadThrow m => Header -> m S.ByteString
packHeader header = do
    (left, right) <- packHeaderNoChecksum header
    let sumsl :: SL.ByteString -> Int
        sumsl = SL.foldl' (\ !acc !v -> acc + fromIntegral v) 0
    encChecksum <- encodeOctal 7 $ sumsl left + 32 * 8 + sumsl right
    return $
        SL.toStrict $
        toLazyByteString $ mconcat [lazyByteString left, encChecksum, word8 0, lazyByteString right]


packHeaderNoChecksum :: MonadThrow m => Header -> m (SL.ByteString, SL.ByteString)
packHeaderNoChecksum Header {..} = do
    let CTime headerTime' = headerTime
    hNameSuffix <- encodeShort 100 headerFileNameSuffix
    hFileMode <- encodeOctal 7 headerFileMode
    hOwnerId <- encodeOctal 7 headerOwnerId
    hGroupId <- encodeOctal 7 headerGroupId
    hPayloadSize <- encodeOctal 11 headerPayloadSize
    hTime <- encodeOctal 11 headerTime'
    hLinkName <- encodeShort 100 headerLinkName
    hOwnerName <- encodeShort 32 headerOwnerName
    hGroupName <- encodeShort 32 headerGroupName
    hDevMajor <- encodeDevice headerDeviceMajor
    hDevMinor <- encodeDevice headerDeviceMinor
    hNamePrefix <- encodeShort 155 headerFileNamePrefix
    return
        ( toLazyByteString $
          mconcat
              [ hNameSuffix
              , hFileMode
              , word8 0
              , hOwnerId
              , word8 0
              , hGroupId
              , word8 0
              , hPayloadSize
              , word8 0
              , hTime
              , word8 0
              ]
        , toLazyByteString $
          mconcat
              [ word8 headerLinkIndicator
              , hLinkName
              , byteString $ S8.pack "ustar\NUL00"
              , hOwnerName
              , hGroupName
              , hDevMajor
              , word8 0
              , hDevMinor
              , word8 0
              , hNamePrefix
              , byteString $ S.replicate 12 0
              ])
  where
    encodeDevice 0 = return $ byteString $ S.replicate 7 0
    encodeDevice devid = encodeOctal 7 devid


-- | Encode a `ShortByteString` with an exact length, NUL terminating if it is
-- shorter, but throwing `TarCreationError` if it is longer.
encodeShort :: MonadThrow m => Int -> ShortByteString -> m Builder
encodeShort !len !sbs
    | lenShort <= len = return $ byteString $ fst $ S.unfoldrN len maybeShort 0
    | otherwise =
        throwM $
        TarCreationError $ "Can't fit '" ++ S8.unpack (SS.fromShort sbs) ++ "' into the tar header"
  where
    lenShort = SS.length sbs
    maybeShort !i
        | i < lenShort = Just (SS.index sbs i, i + 1)
        | otherwise = Just (0, i + 1)


-- | Encode a number in 8base padded with zeros. Throws `TarCreationError` when overflows.
encodeOctal :: (Show a, Integral a, MonadThrow m) => Int -> a -> m Builder
encodeOctal !len !val =
    byteString . S.reverse <$>
    case S.unfoldrN len toOctal val of
        (valStr, Just 0) -> return valStr
        over -> throwM $ TarCreationError $ "<encodeOctal>: Tar value overflow: " ++ show over
  where
    toOctal x =
        let !(q, r) = x `quotRem` 8
        in Just (fromIntegral r + 48, q)


-- | Add a single file into the tarball. Returns the new offset where next entry can be placed.
addFileTar
  :: MonadResource m =>
     FileOffset -> FileInfo -> ConduitM i S8.ByteString m FileOffset
addFileTar offset fi = do
    header <- headerFromFileInfo offset fi
    packHeader header >>= yield
    sourceFile $ filePath fi
    let offset' = offset + blockSize + fileSize fi
        pad = blockSize * (1 + (offset' `div` blockSize)) - offset'
    yield $ S.replicate (fromIntegral pad) 0
    return (offset' + pad)


-- | Add a directory recursively into the tarball. Returns the new offset
-- where next entry can be placed.
addDirectoryTar :: MonadResource m =>
                   FileOffset -> FileInfo -> ConduitM i S8.ByteString m FileOffset
addDirectoryTar offset fi = do
    header <- headerFromFileInfo offset fi
    packHeader header >>= yield
    sourceDirectory (filePath fi) .| addFilesTarConduit (offset + blockSize)


-- | Add a directory recursively into the tarball. Returns the new offset
-- where next entry can be placed.
addSymbolicLinkTar :: MonadResource m =>
                      FileOffset -> FileInfo -> ConduitM i S8.ByteString m FileOffset
addSymbolicLinkTar offset fi = do
    header <- headerFromFileInfo offset fi
    packHeader header >>= yield
    return (offset + blockSize)


addFilesTarConduit :: MonadResource m => FileOffset -> ConduitM FilePath S.ByteString m FileOffset
addFilesTarConduit offset = do
    mfp <- await
    case mfp of
        Just fp -> do
            fi <- liftIO $ getFileInfo fp
            offset' <-
                case fileType fi of
                    FTNormal       -> addFileTar offset fi
                    FTSymbolicLink -> addSymbolicLinkTar offset fi
                    FTDirectory    -> addDirectoryTar offset fi
                    fty            ->
                        throwM $ TarCreationError $ "Unsupported file type: " ++ show fty
            addFilesTarConduit offset'
        Nothing -> return offset

tarConduit :: MonadResource m => ConduitM FilePath S.ByteString m FileOffset
tarConduit = do
  let pad = 2 * blockSize -- two blocks are the terminator for the tar file
  offset <- addFilesTarConduit 0
  yield $ S.replicate (fromIntegral pad) 0
  return (offset + pad)


createTar :: FilePath -> [FilePath] -> IO ()
createTar tarfp dirs = do
  let dirs' = nub $ sort $ map normalise dirs
  runConduitRes $ yieldMany dirs .| void tarConduit .| sinkFile tarfp
