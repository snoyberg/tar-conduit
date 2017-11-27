{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-| This module is about stream-processing tar archives. It is currently
not very well tested. See the documentation of 'withEntries' for an usage sample.
-}
module Data.Conduit.Tar
    ( -- * Basic functions
      tar
    , untar
    , restoreFile
    , restoreFileInto
    , withEntry
    , withEntries
      -- * Helper functions
    , headerFileType
    , headerFilePath
      -- ** Creation
    , tarFilePath
    , filePathConduit
      -- * Directly on files
    , createTarball
    , extractTarball
      -- * Types
    , module Data.Conduit.Tar.Types
    ) where

import Conduit
import Control.Exception (Exception, assert)
import Control.Monad (unless, when, void)
import Control.Monad.Catch (MonadCatch, try)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Foldable (foldr')
import Data.Bits ((.&.))
import qualified Data.ByteString        as S
import Data.ByteString.Builder
import qualified Data.ByteString.Char8  as S8
import qualified Data.ByteString.Short  as SS
import qualified Data.ByteString.Lazy as SL
import qualified Data.ByteString.Unsafe as BU
import Foreign.C.Types (CTime(..))
import System.Posix.Types (CMode)
import System.FilePath
import System.Directory  (getCurrentDirectory, createDirectoryIfMissing)
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
        50 -> FTSymbolicLink (S8.unpack (fromShort (headerLinkName h)))
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
        , headerMagicVersion   = toShort $ S.take 8 $ S.drop 257 bs
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
                let (x, y) = S.splitAt (fromIntegral (min size (fromIntegral (maxBound :: Int)))) bs
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
        Just x@ChunkHeader {} -> leftover x
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

Note that the benefits of stream processing are easily lost when working with a 'Consumer'. For example, the following implementation would have used an unbounded amount of memory:

>         hashentry hdr = when (CT.headerFileType hdr == CT.FTNormal) $ do
>             content <- mconcat <$> sinkList
>             yield (CT.headerFilePath hdr, hash content)
-}
withEntries :: MonadThrow m
            => (Header -> ConduitM ByteString o m ())
            -> ConduitM TarChunk o m ()
withEntries = peekForever . withEntry


-- | Process a single tar entry. See 'withEntries' for a more low level tar processing. Currently
-- this function has full suppoort of ustar format and a minimal support for GNU tar:
--
-- * Long file names 'L'
-- * It discards sparse files 'S'
-- * Ignores any other custom header types
--
withFileInfo :: MonadThrow m
             => (FileInfo -> ConduitM ByteString o m ())
             -> ConduitM TarChunk o m ()
withFileInfo inner = do
    mc <- await
    case mc of
        Nothing -> return ()
        Just (ChunkHeader h)
            | headerLinkIndicator h >= 55 -> do
                when (headerMagicVersion h == gnuTarMagicVersion) $ handleGnuTarHeader h
                withFileInfo inner -- silently skip unsupported headers
        Just (ChunkHeader h) -> do
            payloadsConduit .| (inner (fileInfoFromHeader h) <* sinkNull)
        Just x@(ChunkPayload offset _bs) -> do
            leftover x
            throwM $ UnexpectedPayload offset
        Just (ChunkException e) -> throwM e


-- | Take care of custom GNU tar format.
handleGnuTarHeader :: MonadThrow m
                   => Header
                   -> ConduitM TarChunk o m ()
handleGnuTarHeader h = do
    case headerLinkIndicator h of
        76 -> do
            let pSize = headerPayloadSize h
            -- guard against names that are too long in order to prevent a DoS attack on unbounded
            -- file names
            unless (0 < pSize && pSize <= 4096) $
                throwM $
                FileTypeError (headerPayloadOffset h) 'L' $ "Filepath is too long: " ++ show pSize
            longFileName <-
                SL.toStrict . SL.init . toLazyByteString <$> (payloadsConduit .| sinkBuilder)
            mcNext <- await
            case mcNext of
                Just (ChunkHeader nh) -> do
                    unless (S.isPrefixOf (fromShort (headerFileNameSuffix nh)) longFileName) $
                        throwM $
                        FileTypeError (headerPayloadOffset nh) 'L' $
                        "Long filename doesn't match the original."
                    leftover
                        (ChunkHeader $
                         nh
                         { headerFileNameSuffix = toShort longFileName
                         , headerFileNamePrefix = SS.empty
                         })
                Just c@(ChunkPayload offset _) -> do
                    leftover c
                    throwM $ InvalidHeader offset
                Nothing -> throwM NoMoreHeaders
        83 -> do
            payloadsConduit .| sinkNull -- discard sparse files payload
            -- TODO : Implement restoring of sparse files
        _ -> return ()


-- | Extract tarball
untar :: MonadThrow m
      => (FileInfo -> ConduitM ByteString o m ())
      -> ConduitM ByteString o m ()
untar inner = untarChunks .| withFileInfo inner



--------------------------------------------------------------------------------
-- Create a tar file -----------------------------------------------------------
--------------------------------------------------------------------------------

gnuTarMagicVersion = toShort (S8.pack "ustar  \NUL")

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


headerFromFileInfo :: MonadThrow m =>
                      FileOffset -- ^ Starting offset within the tarball. Must
                      -- be multiple of 512, otherwise error.
                   -> FileInfo -- ^ File info.
                   -> m Header
headerFromFileInfo offset fi = do
    unless (offset `mod` 512 == 0) $
        throwM $ TarCreationError $ "Offset must always be a multiple of 512"
    let (prefix, suffix) = splitPathAt 100 $ filePath fi
    unless (SS.length prefix < 155) $ throwM $ FileNameTooLong fi
    (payloadSize, linkName, linkIndicator) <-
        case fileType fi of
          FTNormal          -> return (fileSize fi, SS.empty, 48)
          FTSymbolicLink ln -> return (0, toShort (S8.pack ln), 50)
          FTDirectory       -> return (0, SS.empty, 53)
          fty               -> throwM $ TarCreationError $ "Unsupported file type: " ++ show fty
    return Header
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
    | S.length fp < n = (SS.empty, toShort fp)
    | otherwise =
        let sfp = S8.splitWith isPathSeparator fp
            toShortPath = toShort . S8.intercalate pathSeparatorS
            sepWith p (tlen, prefix, suffix) =
                case tlen + S.length p of
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
        toLazyByteString $ lazyByteString left <> encChecksum <> word8 0 <> lazyByteString right


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
    hMagicVersion <- encodeShort 8 headerMagicVersion
    hOwnerName <- encodeShort 32 headerOwnerName
    hGroupName <- encodeShort 32 headerGroupName
    hDevMajor <- encodeDevice headerDeviceMajor
    hDevMinor <- encodeDevice headerDeviceMinor
    hNamePrefix <- encodeShort 155 headerFileNamePrefix
    return
        ( toLazyByteString $
          hNameSuffix <>
          hFileMode <> word8 0 <>
          hOwnerId <> word8 0 <>
          hGroupId <> word8 0 <>
          hPayloadSize <> word8 0 <>
          hTime <> word8 0
        , toLazyByteString $
          word8 headerLinkIndicator <>
          hLinkName <>
          hMagicVersion <>
          hOwnerName <>
          hGroupName <>
          hDevMajor <> word8 0 <>
          hDevMinor <> word8 0 <>
          hNamePrefix <>
          byteString (S.replicate 12 0)
        )
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
        TarCreationError $ "Can't fit '" ++ S8.unpack (fromShort sbs) ++ "' into the tar header"
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
           -> Header -- ^ Header for the file that we are currently recieving the payload for
           -> (FileOffset -> ConduitM (Either a ByteString) ByteString m FileOffset)
           -- ^ Continuation for after all payload has been received
           -> ConduitM (Either a ByteString) ByteString m FileOffset
tarPayload size header cont
    | size == headerPayloadSize header = cont (headerOffset header + blockSize)
    | otherwise = do
        eContent <- await
        case eContent of
            Just h@(Left _) -> do
                leftover h
                throwM $ TarCreationError "Not enough payload."
            Just (Right content) -> do
                let size' = size + fromIntegral (S.length content)
                unless (size' <= headerPayloadSize header) $
                    throwM $ TarCreationError "Too much payload"
                yield content
                if size' == headerPayloadSize header
                    then do
                        paddedSize <- yieldNulPadding size'
                        cont (headerPayloadOffset header + paddedSize)
                    else tarPayload size' header cont
            Nothing -> throwM $ TarCreationError "Stream finished abruptly. Not enough payload."



tarFileInfo :: (MonadCatch m, MonadThrow m) =>
               FileOffset -> ConduitM (Either FileInfo ByteString) ByteString m FileOffset
tarFileInfo offset = do
    eContent <- await
    case eContent of
        Just (Right c) ->
            throwM $ TarCreationError "Received payload without a corresponding FileInfo."
        Just (Left fi) -> do
            eHeader <- try $ headerFromFileInfo offset fi
            case eHeader of
                Left (FileNameTooLong _) -> do
                    let fPath = filePath fi
                        fPathLen = fromIntegral (S.length fPath + 1)
                        pad =
                            case fPathLen `mod` blockSize of
                                0 -> 0
                                x -> blockSize - x
                    header <-
                        headerFromFileInfo (offset + blockSize + fPathLen + pad) $
                        fi {filePath = S.take 100 fPath}
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





-- | Create a tar archive by suppying `FileInfo`, which must be immediately
-- followed by the file content, wheneve its type is `FTNormal`.
tar :: (MonadCatch m, MonadResource m) =>
       ConduitM (Either FileInfo ByteString) ByteString m FileOffset
tar = do
    offset <- tarFileInfo 0
    yield terminatorBlock
    return $ offset + fromIntegral (S.length terminatorBlock)




tarHeader :: MonadThrow m =>
             FileOffset -> ConduitM (Either Header ByteString) ByteString m FileOffset
tarHeader offset = do
    eContent <- await
    case eContent of
        Just c@(Right _) -> do
            leftover c
            throwM $ TarCreationError "Received payload without a corresponding Header."
        Just (Left header) -> do
            packHeader header >>= yield
            tarPayload 0 header tarHeader
        Nothing -> do
            yield terminatorBlock
            return $ offset + fromIntegral (S.length terminatorBlock)


-- | Turn a stream of file paths into a stream of `FileInfo` and file
-- conent. All paths will be decended into recursively.
filePathConduit :: MonadResource m =>
                   ConduitM FilePath (Either FileInfo ByteString) m ()
filePathConduit = do
    mfp <- await
    case mfp of
        Just fp -> do
            fi <- liftIO $ getFileInfo fp
            case fileType fi of
                    FTNormal         -> do
                        yield (Left fi)
                        sourceFile (S8.unpack (filePath fi)) .| mapC Right
                    FTSymbolicLink _ ->
                        yield (Left fi)
                    FTDirectory      -> do
                        yield (Left fi)
                        sourceDirectory (S8.unpack (filePath fi)) .| filePathConduit
                    fty              -> do
                        leftover fp
                        throwM $ TarCreationError $ "Unsupported file type: " ++ show fty
            filePathConduit
        Nothing -> return ()


-- | Recursively tar all of the files and directories.
tarFilePath :: (MonadCatch m, MonadResource m) => ConduitM FilePath ByteString m FileOffset
tarFilePath = filePathConduit .| tar



createTarball :: FilePath -- ^ File name for the tarball
              -> [FilePath] -- ^ List of files and directories to include in the tarball
              -> IO ()
createTarball tarfp dirs = do
    runConduitRes $ yieldMany dirs .| void tarFilePath .| sinkFile tarfp


pathSeparatorS = S8.singleton pathSeparator

fileInfoFromHeader :: Header -> FileInfo
fileInfoFromHeader header@(Header {..}) =
    FileInfo
    { filePath =
          fromShort headerFileNamePrefix <> pathSeparatorS <>
          fromShort headerFileNameSuffix
    , fileUserId = headerOwnerId
    , fileUserName = fromShort headerOwnerName
    , fileGroupId = headerGroupId
    , fileGroupName = fromShort headerGroupName
    , fileMode = headerFileMode
    , fileSize = headerPayloadSize
    , fileType = headerFileType header
    , fileModTime = headerTime
    }



extractTarball :: FilePath -- ^ Filename for the tarball
               -> Maybe FilePath -- ^ Folder where tarball should be extract
                                 -- to. Default is the current path
               -> IO ()
extractTarball tarfp mcd = do
    cd <- maybe getCurrentDirectory return mcd
    createDirectoryIfMissing True cd
    runConduitRes $ sourceFileBS tarfp .| untar (restoreFileInto cd)


-- | Restore all files into a folder. Absolute file paths will be turned into
-- relative to the supplied folder.
restoreFileInto :: MonadResource m =>
                   FilePath -> FileInfo -> ConduitM ByteString o m ()
restoreFileInto cd fi =
    restoreFile fi {filePath = S8.pack (cd </> makeRelative "/" (S8.unpack (filePath fi)))}
