{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| This module is about stream-processing tar archives. It is currently
not very well tested. See the documentation of 'withEntries' for an usage sample.
-}
module Data.Conduit.Tar
    ( -- * Basic functions
      tar
    , tarEntries
    , untar
    , untarWithFinalizers
    , untarWithExceptions
    , restoreFile
    , restoreFileInto
    , restoreFileIntoLenient
    , restoreFileWithErrors
    -- ** Operate on Chunks
    , untarChunks
    , withEntry
    , withEntries
    , withFileInfo
      -- * Helper functions
    , headerFileType
    , headerFilePath
    , getFileInfo
    , makeDirectory
      -- ** Creation
    , tarFiles
    , tarFilePath
    , filePathConduit
      -- * Directly on files
    , createTarball
    , writeTarball
    , extractTarball
    , extractTarballLenient
      -- * Types
    , module Data.Conduit.Tar.Types
    ) where

import           Conduit                  as C
import           Control.Exception        (SomeException, assert)
import           Control.Monad            (unless, void, when)
import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8    as S8
import qualified Data.ByteString.Lazy     as SL
import           Data.ByteString.Short    (ShortByteString, fromShort, toShort)
import qualified Data.ByteString.Short    as SS
import qualified Data.ByteString.Unsafe   as BU
import           Data.Foldable            (foldlM, foldr')
import           Data.Int
import           Data.Monoid              (mempty, (<>))
import qualified Data.Set                 as Set
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           Foreign.C.Types          (CTime (..))
import           Foreign.Storable
import           System.Directory         (createDirectoryIfMissing,
                                           getCurrentDirectory)
import           System.FilePath
import qualified System.FilePath.Posix    as Posix
import           System.IO

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative      ((<*))
#endif

import           Data.Conduit.Tar.Types
#ifdef WINDOWS
import           Data.Conduit.Tar.Windows
#else
import           Data.Conduit.Tar.Unix
#endif


headerFilePathBS :: Header -> S.ByteString
headerFilePathBS Header {..} =
    if SS.null headerFileNamePrefix
        then fromShort headerFileNameSuffix
        else S.concat
                 [fromShort headerFileNamePrefix, pathSeparatorS, fromShort headerFileNameSuffix]

-- | Construct a `FilePath` from `headerFileNamePrefix` and `headerFileNameSuffix`.
--
-- @since 0.1.0
headerFilePath :: Header -> FilePath
headerFilePath = decodeFilePath . headerFilePathBS

-- | Get Header file type.
--
-- @since 0.1.0
headerFileType :: Header -> FileType
headerFileType h =
    case headerLinkIndicator h of
        0  -> FTNormal
        48 -> FTNormal
        49 -> FTHardLink (fromShort (headerLinkName h))
        50 -> FTSymbolicLink (fromShort (headerLinkName h))
        51 -> FTCharacterSpecial
        52 -> FTBlockSpecial
        53 -> FTDirectory
        54 -> FTFifo
        x  -> FTOther x

parseHeader :: FileOffset -> ByteString -> Either TarException Header
parseHeader offset bs = do
    unless (S.length bs == 512) $ Left $ IncompleteHeader offset
    let checksumBytes = BU.unsafeTake 8 $ BU.unsafeDrop 148 bs
        expectedChecksum = parseOctal checksumBytes
        actualChecksum = bsum bs - bsum checksumBytes + 8 * space
        magicVersion = toShort $ BU.unsafeTake 8 $ BU.unsafeDrop 257 bs
        getNumber :: (Storable a, Bits a, Integral a) => Int -> Int -> a
        getNumber = if magicVersion == gnuTarMagicVersion then getHexOctal else getOctal

    unless (actualChecksum == expectedChecksum) (Left (BadChecksum offset))
    return Header
        { headerOffset         = offset
        , headerPayloadOffset  = offset + 512
        , headerFileNameSuffix = getShort 0 100
        , headerFileMode       = getOctal 100 8
        , headerOwnerId        = getNumber 108 8
        , headerGroupId        = getNumber 116 8
        , headerPayloadSize    = getNumber 124 12
        , headerTime           = CTime $ getNumber 136 12
        , headerLinkIndicator  = BU.unsafeIndex bs 156
        , headerLinkName       = getShort 157 100
        , headerMagicVersion   = magicVersion
        , headerOwnerName      = getShort 265 32
        , headerGroupName      = getShort 297 32
        , headerDeviceMajor    = getNumber 329 8
        , headerDeviceMinor    = getNumber 337 8
        , headerFileNamePrefix = getShort 345 155
        }
  where
    bsum :: ByteString -> Int
    bsum = S.foldl' (\c n -> c + fromIntegral n) 0

    getShort off len = toShort $ S.takeWhile (/= 0) $ BU.unsafeTake len $ BU.unsafeDrop off bs

    getOctal :: Integral a => Int -> Int -> a
    getOctal off len = parseOctal $ BU.unsafeTake len $ BU.unsafeDrop off bs

    -- | Depending on the first bit of the first byte in the range either choose direct
    -- hex representation, or classic octal string view.
    getHexOctal :: (Storable a, Bits a, Integral a) => Int -> Int -> a
    getHexOctal off len = if BU.unsafeIndex bs off .&. 0x80 == 0x80
                          then fromHex $ BU.unsafeTake len $ BU.unsafeDrop off bs
                          else getOctal off len

    parseOctal :: Integral i => ByteString -> i
    parseOctal = S.foldl' (\t c -> t * 8 + fromIntegral (c - zero)) 0
               . S.takeWhile (\c -> zero <= c && c <= seven)
               . S.dropWhile (== space)

    space :: Integral i => i
    space = 0x20
    zero = 48
    seven = 55

-- | Make sure we don't use more bytes than we can fit in the data type.
fromHex :: forall a . (Storable a, Bits a, Integral a) => ByteString -> a
fromHex str = S.foldl' (\ acc x -> (acc `shiftL` 8) .|. fromIntegral x) 0 $
              S.drop (max 0 (S.length str - sizeOf (undefined :: a))) str



-- | Convert a stream of raw bytes into a stream of 'TarChunk's. This stream can further be passed
-- into `withFileInfo` or `withHeaders` functions.
--
-- @since 0.2.1
untarChunks :: Monad m => ConduitM ByteString TarChunk m ()
untarChunks =
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
                                    x   -> x)
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
                let (x, y) = S.splitAt (fromIntegral (min size (fromIntegral (maxBound :: Int)))) bs
                yield $ ChunkPayload offset x
                let size' = size - fromIntegral (S.length x)
                    offset' = offset + fromIntegral (S.length x)
                unless (S.null y) (leftover y)
                payloads offset' size'


-- | Process a single tar entry. See 'withEntries' for more details.
--
-- @since 0.1.0
--
withEntry :: MonadThrow m
          => (Header -> ConduitM ByteString o m r)
          -> ConduitM TarChunk o m r
withEntry inner = do
    mc <- await
    case mc of
        Nothing -> throwM NoMoreHeaders
        Just (ChunkHeader h) -> payloadsConduit .| (inner h <* sinkNull)
        Just x@(ChunkPayload offset _bs) -> do
            leftover x
            throwM $ UnexpectedPayload offset
        Just (ChunkException e) -> throwM e


payloadsConduit :: MonadThrow m
               => ConduitM TarChunk ByteString m ()
payloadsConduit = do
    mx <- await
    case mx of
        Just (ChunkPayload _ bs) -> yield bs >> payloadsConduit
        Just x@ChunkHeader {}    -> leftover x
        Just (ChunkException e)  -> throwM e
        Nothing                  -> return ()


{-| This function handles each entry of the tar archive according to the
behaviour of the function passed as first argument.

Here is a full example function, that reads a compressed tar archive and for each entry that is a
simple file, it prints its file path and SHA256 digest. Note that this function can throw
exceptions!

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
>                                .| CT.untarChunks           -- decode the tar archive
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

Note that the benefits of stream processing are easily lost when working with a 'Consumer'. For
example, the following implementation would have used an unbounded amount of memory:

>         hashentry hdr = when (CT.headerFileType hdr == CT.FTNormal) $ do
>             content <- mconcat <$> sinkList
>             yield (CT.headerFilePath hdr, hash content)

@since 0.1.0
-}
withEntries :: MonadThrow m
            => (Header -> ConduitM ByteString o m ())
            -> ConduitM TarChunk o m ()
withEntries = peekForever . withEntry


-- | Extract a tarball, similarly to `withEntries`, but instead of dealing directly with tar format,
-- this conduit allows you to work directly on file abstractions `FileInfo`. For now support is
-- minimal:
--
-- * Old v7 tar format.
-- * ustar: POSIX 1003.1-1988 format
-- * and only some portions of GNU format:
--   * Larger values for `fileUserId`, `fileGroupId`, `fileSize` and `fileModTime`.
--   * 'L' type - long file names, but only up to 4096 chars to prevent DoS attack
--   * other types are simply discarded
--
-- /Note/ - Here is a really good reference for specifics of different tar formats:
-- <https://github.com/libarchive/libarchive/wiki/ManPageTar5>
--
-- @since 0.2.2
withFileInfo :: MonadThrow m
             => (FileInfo -> ConduitM ByteString o m ())
             -> ConduitM TarChunk o m ()
withFileInfo inner = start
  where
    start = await >>= maybe (return ()) go
    go x =
        case x of
            ChunkHeader h
                | headerLinkIndicator h >= 55 ->
                    if headerMagicVersion h == gnuTarMagicVersion
                        then handleGnuTarHeader h >>= maybe start go
                        else dropWhileC
                                 (\case
                                      ChunkPayload _ _ -> True
                                      _                -> False) >> start
            ChunkHeader h -> do
                payloadsConduit .| (inner (fileInfoFromHeader h) <* sinkNull)
                start
            ChunkPayload offset _bs -> do
                leftover x
                throwM $ UnexpectedPayload offset
            ChunkException e -> throwM e


-- | Take care of custom GNU tar format.
handleGnuTarHeader :: MonadThrow m
                   => Header
                   -> ConduitM TarChunk o m (Maybe TarChunk)
handleGnuTarHeader h =
    case headerLinkIndicator h of
        76 -> do
            let pSize = headerPayloadSize h
            -- guard against names that are too long in order to prevent a DoS attack on unbounded
            -- file names
            unless (0 < pSize && pSize <= 4096) $
                throwM $
                FileTypeError (headerPayloadOffset h) 'L' $ "Filepath is too long: " ++ show pSize
            longFileNameBuilder <- payloadsConduit .| foldMapC byteString
            let longFileName = SL.toStrict . SL.init . toLazyByteString $ longFileNameBuilder
            mcNext <- await
            case mcNext of
                Just (ChunkHeader nh) -> do
                    unless (S.isPrefixOf (fromShort (headerFileNameSuffix nh)) longFileName) $
                        throwM $
                        FileTypeError (headerPayloadOffset nh) 'L'
                        "Long filename doesn't match the original."
                    return
                        (Just $ ChunkHeader $
                         nh
                         { headerFileNameSuffix = toShort longFileName
                         , headerFileNamePrefix = SS.empty
                         })
                Just c@(ChunkPayload offset _) -> do
                    leftover c
                    throwM $ InvalidHeader offset
                Just (ChunkException exc) -> throwM exc
                Nothing -> throwM NoMoreHeaders
        83 -> do
            payloadsConduit .| sinkNull -- discard sparse files payload
            -- TODO : Implement restoring of sparse files
            return Nothing
        _ -> return Nothing



-- | Just like `withFileInfo`, but works directly on the stream of bytes.
--
-- @since 0.2.0
untar :: MonadThrow m
      => (FileInfo -> ConduitM ByteString o m ())
      -> ConduitM ByteString o m ()
untar inner = untarChunks .| withFileInfo inner


-- | Just like `untar`, except that each `FileInfo` handling function can produce a finalizing
-- action, all of which will be executed after the whole tarball has been processed in the opposite
-- order. Very useful with `restoreFile` and `restoreFileInto`, since they restore direcory
-- modification timestamps only after files have been fully written to disk.
--
-- @since 0.2.0
untarWithFinalizers ::
       (MonadThrow m, MonadIO m)
    => (FileInfo -> ConduitM ByteString (IO ()) m ())
    -> ConduitM ByteString c m ()
untarWithFinalizers inner = do
    finilizers <- untar inner .| foldlC (>>) (return ())
    liftIO finilizers


-- | Same as `untarWithFinalizers`, but will also produce a list of any exceptions that might have
-- occured during restoration process.
--
-- @since 0.2.5
untarWithExceptions ::
       (MonadThrow m, MonadIO m)
    => (FileInfo -> ConduitM ByteString (IO (FileInfo, [SomeException])) m ())
    -> ConduitM ByteString c m [(FileInfo, [SomeException])]
untarWithExceptions inner = do
    finalizers <- untar inner .| C.foldMapC (fmap pure)
    filter (not . null . snd) <$> liftIO finalizers


--------------------------------------------------------------------------------
-- Create a tar file -----------------------------------------------------------
--------------------------------------------------------------------------------

gnuTarMagicVersion :: ShortByteString
gnuTarMagicVersion = toShort (S8.pack "ustar  \NUL")

ustarMagicVersion :: ShortByteString
ustarMagicVersion = toShort (S8.pack "ustar\NUL00")

blockSize :: FileOffset
blockSize = 512

terminatorBlock :: ByteString
terminatorBlock = S.replicate (fromIntegral (2 * blockSize)) 0

defHeader :: FileOffset -> Header
defHeader offset = Header
          { headerOffset = offset
          , headerPayloadOffset = offset + 512
          , headerFileNameSuffix = SS.empty
          , headerFileMode = 0o644
          , headerOwnerId = 0
          , headerGroupId = 0
          , headerPayloadSize = 0
          , headerTime = 0
          , headerLinkIndicator = 0
          , headerLinkName = SS.empty
          , headerMagicVersion = ustarMagicVersion
          , headerOwnerName = "root"
          , headerGroupName = "root"
          , headerDeviceMajor = 0
          , headerDeviceMinor = 0
          , headerFileNamePrefix = SS.empty
          }


headerFromFileInfo ::
       MonadThrow m
    => FileOffset -- ^ Starting offset within the tarball. Must be multiple of 512, otherwise error.
    -> FileInfo -- ^ File info.
    -> m (Either TarCreateException Header)
headerFromFileInfo offset fi = do
    unless (offset `mod` 512 == 0) $
        throwM $
        TarCreationError $
        "<headerFromFileInfo>: Offset must always be a multiple of 512 for file: " ++
        getFileInfoPath fi
    let (prefix, suffix) = splitPathAt 100 $ filePath fi
    if SS.length prefix > 155 || SS.null suffix
        then return $ Left $ FileNameTooLong fi
        else do
            (payloadSize, linkName, linkIndicator) <-
                case fileType fi of
                    FTNormal -> return (fileSize fi, SS.empty, 48)
                    FTHardLink ln -> return (0, toShort ln, 49)
                    FTSymbolicLink ln -> return (0, toShort ln, 50)
                    FTDirectory -> return (0, SS.empty, 53)
                    fty ->
                        throwM $
                        TarCreationError $
                        "<headerFromFileInfo>: Unsupported file type: " ++
                        show fty ++ " for file: " ++ getFileInfoPath fi
            return $
                Right
                    Header
                    { headerOffset = offset
                    , headerPayloadOffset = offset + 512
                    , headerFileNameSuffix = suffix
                    , headerFileMode = fileMode fi
                    , headerOwnerId = fileUserId fi
                    , headerGroupId = fileGroupId fi
                    , headerPayloadSize = payloadSize
                    , headerTime = fileModTime fi
                    , headerLinkIndicator = linkIndicator
                    , headerLinkName = linkName
                    , headerMagicVersion = ustarMagicVersion
                    , headerOwnerName = toShort $ fileUserName fi
                    , headerGroupName = toShort $ fileGroupName fi
                    , headerDeviceMajor = 0
                    , headerDeviceMinor = 0
                    , headerFileNamePrefix = prefix
                    }


-- | Split a file path at the @n@ mark from the end, while still keeping the
-- split as a valid path, i.e split at a path separator only.
splitPathAt :: Int -> ByteString -> (ShortByteString, ShortByteString)
splitPathAt n fp
    | S.length fp <= n = (SS.empty, toShort fp)
    | otherwise =
        let sfp = S8.splitWith isPathSeparator fp
            sepWith p (tlen, prefix', suffix') =
                case S.length p + 1 + tlen of
                    tlen'
                        | tlen' <= n -> (tlen', prefix', p : suffix')
                    tlen' -> (tlen', p : prefix', suffix')
            (_, prefix, suffix) = foldr' sepWith (0, [], []) sfp
            toShortPath = toShort . S8.intercalate pathSeparatorS
        in (toShortPath prefix, toShortPath suffix)

packHeader :: MonadThrow m => Header -> m S.ByteString
packHeader header = do
    (left, right) <- packHeaderNoChecksum header
    let sumsl :: SL.ByteString -> Int
        sumsl = SL.foldl' (\ !acc !v -> acc + fromIntegral v) 0
        checksum = sumsl left + 32 * 8 + sumsl right
    encChecksum <-
        either
            (\(_, val) ->
                 throwM $
                 TarCreationError $
                 "<packHeader>: Impossible happened - Checksum " ++
                 show val ++ " doesn't fit into header for file: " ++ headerFilePath header)
            return $
        encodeOctal 8 checksum
    return $ SL.toStrict $ left <> toLazyByteString encChecksum <> right

packHeaderNoChecksum :: MonadThrow m => Header -> m (SL.ByteString, SL.ByteString)
packHeaderNoChecksum h@Header {..} = do
    let CTime headerTime' = headerTime
        magic0 = headerMagicVersion
    (magic1, hOwnerId) <- encodeNumber magic0 "ownerId" 8 headerOwnerId
    (magic2, hGroupId) <- encodeNumber magic1 "groupId" 8 headerGroupId
    (magic3, hPayloadSize) <- encodeNumber magic2 "payloadSize" 12 headerPayloadSize
    (magic4, hTime) <- encodeNumber magic3 "time" 12 headerTime'
    (magic5, hDevMajor) <- encodeDevice magic4 "Major" headerDeviceMajor
    (magic6, hDevMinor) <- encodeDevice magic5 "Minor" headerDeviceMinor
    hNameSuffix <- encodeShort h "nameSuffix" 100 headerFileNameSuffix
    hFileMode <- throwNumberEither "fileMode" $ encodeOctal 8 headerFileMode
    hLinkName <- encodeShort h "linkName" 100 headerLinkName
    hMagicVersion <- encodeShort h "magicVersion" 8 magic6
    hOwnerName <- encodeShort h "ownerName" 32 headerOwnerName
    hGroupName <- encodeShort h "groupName" 32 headerGroupName
    hNamePrefix <- encodeShort h "namePrefix" 155 headerFileNamePrefix
    return
        ( toLazyByteString $
          hNameSuffix <>
          hFileMode <>
          hOwnerId <>
          hGroupId <>
          hPayloadSize <>
          hTime
        , toLazyByteString $
          word8 headerLinkIndicator <>
          hLinkName <>
          hMagicVersion <>
          hOwnerName <>
          hGroupName <>
          hDevMajor <>
          hDevMinor <>
          hNamePrefix <>
          byteString (S.replicate 12 0)
        )
  where
    encodeNumber magic field len = throwNumberEither field . fallbackHex magic . encodeOctal len
    encodeDevice magic _ 0     = return (magic, byteString $ S.replicate 8 0)
    encodeDevice magic m devid = encodeNumber magic ("device" ++ m) 8 devid
    fallbackHex magic (Right enc)       = Right (magic, enc)
    fallbackHex _     (Left (len, val)) = (,) gnuTarMagicVersion <$> encodeHex len val
    throwNumberEither _     (Right v)         = return v
    throwNumberEither field (Left (len, val)) =
        throwM $
        TarCreationError $
        "<packHeaderNoChecksum>: Tar value overflow for file: " ++
        headerFilePath h ++
        " (for field '" ++ field ++ "' with maxLen " ++ show len ++ "): " ++ show val


-- | Encode a number as hexadecimal with most significant bit set to 1. Returns Left if the value
-- doesn't fit in a ByteString of the supplied length, also prohibits negative numbers if precision
-- of value is higher than available length. Eg. length 8 can't reliably encoed negative numbers,
-- since MSB is already used for flagging Hex extension.
encodeHex :: (Storable a, Bits a, Integral a) =>
             Int -> a -> Either (Int, a) Builder
encodeHex !len !val =
    if complement (complement 0 `shiftL` infoBits) .&. val == val &&
       not (val < 0 && len < sizeOf val)
        then go 0 val mempty
        else Left (len, val)
  where
    len' = len - 1
    infoBits = len * 8 - 1
    go !n !cur !acc
        | n < len' = go (n + 1) (cur `shiftR` 8) (word8 (fromIntegral (cur .&. 0xFF)) <> acc)
        | otherwise = return (word8 (fromIntegral (cur .&. 0x7F) .|. 0x80) <> acc)


-- | Encode a number in 8base padded with zeros and terminated with NUL.
encodeOctal :: (Integral a) =>
                Int -> a -> Either (Int, a) Builder
encodeOctal !len' !val
    | val < 0 = Left (len', val)
    | otherwise = go 0 val (word8 0)
  where
    !len = len' - 1
    go !n !cur !acc
        | cur == 0 =
            if n < len
                then return $ byteString (S.replicate (len - n) 48) <> acc
                else return acc
        | n < len =
            let !(q, r) = cur `quotRem` 8
            in go (n + 1) q (word8 (fromIntegral r + 48) <> acc)
        | otherwise = Left (len', val)



-- | Encode a `ShortByteString` with an exact length, NUL terminating if it is
-- shorter, but throwing `TarCreationError` if it is longer.
encodeShort :: MonadThrow m => Header -> String -> Int -> ShortByteString -> m Builder
encodeShort h field !len !sbs
    | lenShort <= len = return $ shortByteString sbs <> byteString (S.replicate (len - lenShort) 0)
    | otherwise =
        throwM $
        TarCreationError $
        "<encodeShort>: Tar string value overflow for file: " ++
        headerFilePath h ++
        " (for field '" ++ field ++ "' with maxLen " ++ show len ++ "): " ++ S8.unpack (fromShort sbs)
  where
    lenShort = SS.length sbs


-- | Produce a ByteString chunk with NUL characters of the size needed to get up
-- to the next 512 byte mark in respect to the supplied offset and return that
-- offset incremented to that mark.
yieldNulPadding :: Monad m => FileOffset -> ConduitM i ByteString m FileOffset
yieldNulPadding n = do
    let pad = blockSize - (n `mod` blockSize)
    if pad /= blockSize
        then yield (S.replicate (fromIntegral pad) 0) >> return (n + pad)
        else return n



-- | Handle tar payload, while validating its size and padding it to the full
-- block at the end.
tarPayload :: MonadThrow m =>
              FileOffset -- ^ Received payload size
           -> Header -- ^ Header for the file that we are currently receiving the payload for
           -> (FileOffset -> ConduitM (Either a ByteString) ByteString m FileOffset)
           -- ^ Continuation for after all payload has been received
           -> ConduitM (Either a ByteString) ByteString m FileOffset
tarPayload size header cont
    | size == headerPayloadSize header = cont (headerOffset header + blockSize)
    | otherwise = go size
  where
    go prevSize = do
        eContent <- await
        case eContent of
            Just h@(Left _) -> do
                leftover h
                throwM $
                    TarCreationError $
                    "<tarPayload>: Not enough payload for file: " ++ headerFilePath header
            Just (Right content) -> do
                let nextSize = prevSize + fromIntegral (S.length content)
                unless (nextSize <= headerPayloadSize header) $
                    throwM $
                    TarCreationError $
                    "<tarPayload>: Too much payload (" ++
                    show nextSize ++ ") for file with size (" ++
                    show (headerPayloadSize header) ++ "): " ++ headerFilePath header
                yield content
                if nextSize == headerPayloadSize header
                    then do
                        paddedSize <- yieldNulPadding nextSize
                        cont (headerPayloadOffset header + paddedSize)
                    else go nextSize
            Nothing ->
                throwM $
                TarCreationError "<tarPayload>: Stream finished abruptly. Not enough payload."



tarHeader :: MonadThrow m =>
             FileOffset -> ConduitM (Either Header ByteString) ByteString m FileOffset
tarHeader offset = do
    eContent <- await
    case eContent of
        Just (Right bs) | S.null bs -> tarHeader offset -- ignore empty content
        Just c@(Right _) -> do
            leftover c
            throwM $
                TarCreationError "<tarHeader>: Received payload without a corresponding Header."
        Just (Left header) -> do
            packHeader header >>= yield
            tarPayload 0 header tarHeader
        Nothing -> do
            yield terminatorBlock
            return $ offset + fromIntegral (S.length terminatorBlock)



tarFileInfo :: MonadThrow m =>
               FileOffset -> ConduitM (Either FileInfo ByteString) ByteString m FileOffset
tarFileInfo offset = do
    eContent <- await
    case eContent of
        Just (Right bs)
            | S.null bs -> tarFileInfo offset -- ignore empty content
        Just c@(Right _) -> do
            leftover c
            throwM $
                TarCreationError "<tarFileInfo>: Received payload without a corresponding FileInfo."
        Just (Left fi) -> do
            eHeader <- headerFromFileInfo offset fi
            case eHeader of
                Left (FileNameTooLong _) -> do
                    let fPath = filePath fi
                        fPathLen = fromIntegral (S.length fPath + 1)
                        pad =
                            case fPathLen `mod` blockSize of
                                0 -> 0
                                x -> blockSize - x
                    eHeader' <-
                        headerFromFileInfo
                            (offset + blockSize + fPathLen + pad)
                            (fi {filePath = S.take 100 fPath})
                    header <- either throwM return eHeader'
                    pHeader <- packHeader header
                    pFileNameHeader <-
                        packHeader $
                        (defHeader offset)
                        { headerFileNameSuffix = "././@LongLink"
                        , headerPayloadSize = fPathLen
                        , headerLinkIndicator = 76 -- 'L'
                        , headerMagicVersion = gnuTarMagicVersion
                        }
                    yield pFileNameHeader
                    yield fPath
                    yield $ S.replicate (fromIntegral pad + 1) 0
                    yield pHeader
                    tarPayload 0 header tarFileInfo
                Left exc -> throwM exc
                Right header -> do
                    packHeader header >>= yield
                    tarPayload 0 header tarFileInfo
        Nothing -> return offset



-- | Create a tar archive by suppying a stream of `Left` `FileInfo`s. Whenever a
-- file type is `FTNormal`, it must be immediately followed by its content as
-- `Right` `ByteString`. The produced `ByteString` is in the raw tar format and
-- is properly terminated at the end, therefore it can not be extended
-- afterwards. Returned is the total size of the bytestring as a `FileOffset`.
--
-- @since 0.2.0
tar :: MonadThrow m =>
       ConduitM (Either FileInfo ByteString) ByteString m FileOffset
tar = do
    offset <- tarFileInfo 0
    yield terminatorBlock
    return $ offset + fromIntegral (S.length terminatorBlock)


-- | Just like `tar`, except gives you the ability to work at a lower `Header`
-- level, versus more user friendly `FileInfo`. A deeper understanding of tar
-- format is necessary in order to work directly with `Header`s.
--
-- @since 0.2.0
tarEntries :: MonadThrow m =>
              ConduitM (Either Header ByteString) ByteString m FileOffset
tarEntries = do
    offset <- tarHeader 0
    yield terminatorBlock
    return $ offset + fromIntegral (S.length terminatorBlock)

-- | Construct a directory with modification time set to current time and the owner set to @root@.
--
-- @since 0.3.3
makeDirectory :: MonadIO m => FilePath -> m FileInfo
makeDirectory fp = do
    curTime <- floor <$> liftIO getPOSIXTime
    pure
        FileInfo
            { filePath = encodeFilePath fp
            , fileUserId = 0
            , fileUserName = ""
            , fileGroupId = 0
            , fileGroupName = ""
            , fileMode = 0o755
            , fileSize = 0
            , fileModTime = fromIntegral (curTime :: Int64)
            , fileType = FTDirectory
            }


-- | Turn a stream of file paths into a stream of `FileInfo` and file
-- content. All paths will be decended into recursively.
--
-- @since 0.2.0
filePathConduit :: (MonadThrow m, MonadResource m) =>
                   ConduitM FilePath (Either FileInfo ByteString) m ()
filePathConduit = do
    mfp <- await
    case mfp of
        Just fp -> do
            fi <- getFileInfo fp
            case fileType fi of
                FTNormal -> do
                    yield (Left fi)
                    sourceFile (getFileInfoPath fi) .| mapC Right
                FTSymbolicLink _ -> yield (Left fi)
                FTDirectory -> do
                    yield (Left fi)
                    sourceDirectory (getFileInfoPath fi) .| filePathConduit
                fty -> do
                    leftover fp
                    throwM $
                        TarCreationError $
                        "<filePathConduit>: Unsupported file type: " ++
                        show fty ++ " for file: " ++ getFileInfoPath fi
            filePathConduit
        Nothing -> return ()


-- | A smart tarball creator, that can accept files and folders alike, while trying its best to
-- preserve proper tar structure by adding missing directory structures.
--
-- ==== __Examples__
--
-- If executed inside @"tar-conduit"@ package folder, this will produce a tar with all this files
-- and folders:
--
-- >>> runConduitRes (yield "src/Data" .| tarFiles Nothing Nothing (Just "src") .| tar)
--
-- @
-- Data\/
-- Data\/Conduit\/
-- Data\/Conduit\/Tar\/
-- Data\/Conduit\/Tar\/Windows.hs
-- Data\/Conduit\/Tar\/Types.hs
-- Data\/Conduit\/Tar\/Unix.hs
-- Data\/Conduit\/Tar.hs
-- @
--
-- While this will add a prefix to all included files and folders:
--
-- >>> prefix <- makeDirectory "foo/bar"
-- >>> runConduitRes (yield "src/Data/Conduit/Tar" .| tarFiles (Just prefix) Nothing (Just "src/Data/Conduit") .| tar)
--
-- @
-- foo\/
-- foo\/bar\/
-- foo\/bar\/Tar\/
-- foo\/bar\/Tar\/Windows.hs
-- foo\/bar\/Tar\/Types.hs
-- foo\/bar\/Tar\/Unix.hs
-- @
--
-- There is also a way to control how deep should directories be traversed:
--
-- >>> runConduitRes (yield "src/Data" .| tarFiles Nothing (Just 3) (Just "src/Data") .| tar)
--
-- @
-- Conduit\/
-- Conduit\/Tar\/
-- Conduit\/Tar.hs
-- @
--
-- @since 0.3.3
tarFiles ::
       (MonadThrow m, MonadResource m)
    => Maybe FileInfo -- ^ Base directory where all of the files will be placed in. If `Nothing`
                      -- files will be placed in the root of the tarball. Must be of type
                      -- `FTDirectory`, otherwise error.
    -> Maybe Int -- ^ How deep to recurse into the supplied directories, with `Nothing` being as
                 -- deep as possible.
    -> Maybe FilePath -- ^ With respect to which directory to add files, i.e. this prefix path will
                      -- be stripped from all added files. By default it will do no stripping (even
                      -- absolute paths will be stored with leading slash: @/@).
    -> ConduitM FilePath (Either FileInfo ByteString) m ()
tarFiles mbaseDir mDepth mWithRelTo =
    when (maybe True (> 0) mDepth) $ do
        basePath <- maybe (pure "") yieldDirectory mbaseDir
        let mPrefix =
                fmap (encodeFilePath . Posix.normalise . Posix.addTrailingPathSeparator) mWithRelTo
            withoutPrefix dirsCreated fi m =
                case mPrefix of
                    Nothing -> m fi
                    Just "./" -> m fi
                    Just prefix
                        | Just relativePath <- S8.stripPrefix prefix (filePath fi) ->
                            m fi {filePath = relativePath}
                    _ -> pure dirsCreated
            addBaseDir fi = fi {filePath = basePath <> filePath fi}
            yieldFile dirsCreated fi fi' = do
                dirsAdded <-
                    if Set.member (Posix.takeDirectory (getFileInfoPath fi')) dirsCreated
                        then pure Set.empty
                        else yield (Posix.takeDirectory (getFileInfoPath fi)) .|
                             go dirsCreated (Just (1 :: Int))
                yield $ Left $ addBaseDir fi'
                pure (Set.union dirsAdded dirsCreated)
            yieldParentDirs dirsCreated fi fp = do
                let getParentDir =
                        Posix.addTrailingPathSeparator .
                        Posix.takeDirectory . Posix.dropTrailingPathSeparator
                    parentDir = getParentDir fp
                if Set.member parentDir dirsCreated || parentDir == fp
                    then pure mempty
                    else yield (getParentDir (getFileInfoPath fi)) .|
                         go dirsCreated (Just (1 :: Int))
            yieldChildrenDirs mdep dirsCreated fi =
                if maybe True (> 0) mdep
                    then sourceDirectory (getFileInfoPath fi) .| go dirsCreated mdep
                    else pure dirsCreated
            yieldDir mdep dirsCreated fi fi' =
                let fp' = getFileInfoPath fi'
                 in if Set.member fp' dirsCreated
                        then pure dirsCreated
                        else do
                            parents <- yieldParentDirs dirsCreated fi fp'
                            unless (null fp') $ yield $ Left $ addBaseDir fi'
                            let withParents = Set.unions [dirsCreated, parents, Set.singleton fp']
                            yieldChildrenDirs (pred <$> mdep) withParents fi
            go !dirsCreated !mdep = do
                await >>= \case
                    Just fp -> do
                        fi <- getFileInfo fp
                        dirsAdded <-
                            case fileType fi of
                                FTNormal ->
                                    withoutPrefix dirsCreated fi $ \fi' -> do
                                        dirsAdded <- yieldFile dirsCreated fi fi'
                                        sourceFile (getFileInfoPath fi) .| mapC Right
                                        pure dirsAdded
                                FTSymbolicLink _ ->
                                    withoutPrefix dirsCreated fi $ yieldFile dirsCreated fi
                                FTDirectory ->
                                    withoutPrefix dirsCreated fi $ yieldDir mdep dirsCreated fi
                                fty -> do
                                    leftover fp
                                    throwM $
                                        TarCreationError $
                                        "<tarFiles>: Unsupported file type: " ++
                                        show fty ++ " for file: " ++ getFileInfoPath fi
                        go dirsAdded mdep
                    Nothing -> pure dirsCreated
        void $ go (Set.singleton "./") mDepth


-- | Given a directory `FileInfo`, will yield all directory strucutre, eg. @"\/foo\/bar\/baz\/"@,
-- will result in @["\/", "\/foo\/", "\/foo\/bar\/", "\/foo\/bar\/baz\/"]@
yieldDirectory ::
       MonadThrow m => FileInfo -> ConduitT i (Either FileInfo b) m ByteString
yieldDirectory fileInfo =
    case fileType fileInfo of
        FTDirectory ->
            case Posix.splitDirectories $ Posix.normalise fp of
                [] -> throwM $ TarCreationError "<tarFiles>: Base directory name cannot be empty"
                ("/":xs) -> do
                    yield $ Left fileInfo {filePath = "/"}
                    foldlM yieldDirs "/" $ map encodeFilePath xs
                xs -> foldlM yieldDirs "" $ map encodeFilePath xs
        ty ->
            throwM $
            TarCreationError $
            "<tarFiles>: Expected FTDirectory. Unsupported type for directory: " <> show ty
  where
    fp = getFileInfoPath fileInfo
    yieldDirs parent curDirName = do
        let curPath = parent <> curDirName <> pathSeparatorS
        yield $ Left fileInfo {filePath = curPath}
        pure curPath


-- | Recursively tar all of the files and directories. There will be no
-- conversion between relative and absolute paths, so just like with GNU @tar@
-- cli tool, it may be necessary to `setCurrentDirectory` in order to get the
-- paths relative. Using `filePathConduit` directly, while modifying the
-- `filePath`, would be another approach to handling the file paths.
--
-- @since 0.2.0
tarFilePath :: (MonadThrow m, MonadResource m) => ConduitM FilePath ByteString m FileOffset
tarFilePath = filePathConduit .| tar


-- | Uses `tarFilePath` to create a tarball, that will recursively include the
-- supplied list of all the files and directories
--
-- @since 0.2.0
createTarball :: FilePath -- ^ File name for the tarball
              -> [FilePath] -- ^ List of files and directories to include in the tarball
              -> IO ()
createTarball tarfp dirs =
    runConduitRes $ yieldMany dirs .| void tarFilePath .| sinkFile tarfp

-- | Take a list of files and paths, recursively tar them and write output into supplied handle.
--
-- @since 0.2.0
writeTarball :: Handle -- ^ Handle where created tarball will be written to
             -> [FilePath] -- ^ List of files and directories to include in the tarball
             -> IO ()
writeTarball tarHandle dirs =
    runConduitRes $ yieldMany dirs .| void tarFilePath .| sinkHandle tarHandle


-- always use forward slash, see
-- https://github.com/snoyberg/tar-conduit/issues/21
pathSeparatorS :: ByteString
pathSeparatorS = "/" -- S8.singleton pathSeparator


fileInfoFromHeader :: Header -> FileInfo
fileInfoFromHeader header@Header {..} =
    FileInfo
    { filePath = headerFilePathBS header
    , fileUserId = headerOwnerId
    , fileUserName = fromShort headerOwnerName
    , fileGroupId = headerGroupId
    , fileGroupName = fromShort headerGroupName
    , fileMode = headerFileMode
    , fileSize = headerPayloadSize
    , fileType = headerFileType header
    , fileModTime = headerTime
    }


-- | Extract a tarball while using `restoreFileInfo` for writing files onto the file
-- system. Restoration process is cross platform and should work concistently both on Windows and
-- Posix systems.
--
-- @since 0.2.0
extractTarball :: FilePath -- ^ Filename for the tarball
               -> Maybe FilePath -- ^ Folder where tarball should be extract
                                 -- to. Default is the current path
               -> IO ()
extractTarball tarfp mcd = do
    cd <- maybe getCurrentDirectory return mcd
    createDirectoryIfMissing True cd
    runConduitRes $ sourceFileBS tarfp .| untarWithFinalizers (restoreFileInto cd)


prependDirectory :: FilePath -> FileInfo -> FileInfo
prependDirectory cd fi = fi {filePath = prependDir $ getFileInfoPath fi,
                             fileType = prependDirIfNeeded (fileType fi)}
  where
    -- Hard links need to be interpreted based on `cd`, not just CWD, if relative,
    -- otherwise they may point to some invalid location.
    prependDirIfNeeded (FTHardLink p)
        | isRelative $ decodeFilePath p = FTHardLink (prependDir $ decodeFilePath p)
    prependDirIfNeeded other            = other
    prependDir p                        = encodeFilePath (cd </> makeRelative "/" p)


-- | Restore all files into a folder. Absolute file paths will be turned into
-- relative to the supplied folder.
restoreFileInto :: MonadResource m =>
                   FilePath -> FileInfo -> ConduitM ByteString (IO ()) m ()
restoreFileInto cd = restoreFile . prependDirectory cd

-- | Restore all files into a folder. Absolute file paths will be turned into relative to the
-- supplied folder. Yields a list with exceptions instead of throwing them.
--
-- @since 0.2.5
restoreFileIntoLenient :: MonadResource m =>
    FilePath -> FileInfo -> ConduitM ByteString (IO (FileInfo, [SomeException])) m ()
restoreFileIntoLenient cd = restoreFileWithErrors True . prependDirectory cd

-- | Same as `extractTarball`, but ignores possible extraction errors. It can still throw a
-- `TarException` if the tarball is corrupt or malformed.
--
-- @since 0.2.5
extractTarballLenient :: FilePath -- ^ Filename for the tarball
                   -> Maybe FilePath -- ^ Folder where tarball should be extract
                   -- to. Default is the current path
                   -> IO [(FileInfo, [SomeException])]
extractTarballLenient tarfp mcd = do
    cd <- maybe getCurrentDirectory return mcd
    createDirectoryIfMissing True cd
    runConduitRes $
        sourceFileBS tarfp .| untarWithExceptions (restoreFileIntoLenient cd)



-- | Restore files onto the file system. Produces actions that will set the modification time on the
-- directories, which can be executed after the pipeline has finished and all files have been
-- written to disk.
restoreFile :: (MonadResource m) =>
               FileInfo -> ConduitM S8.ByteString (IO ()) m ()
restoreFile fi = restoreFileWithErrors False fi .| mapC void


-- | Restore files onto the file system, much in the same way `restoreFile` does it, except with
-- ability to ignore restoring problematic files and report errors that occured as a list of
-- exceptions, which will be returned as a list when finilizer executed. If a list is empty, it
-- means, that no errors occured and a file only had a finilizer associated with it.
--
-- @since 0.2.4
restoreFileWithErrors ::
       (MonadResource m)
    => Bool -- ^ Lenient flag, results in exceptions thrown instead of collected when set to @False@.
    -> FileInfo
    -> ConduitM S8.ByteString (IO (FileInfo, [SomeException])) m ()
restoreFileWithErrors = restoreFileInternal
