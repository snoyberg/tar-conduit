{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude as P
import Conduit
import Control.Monad (void, when, zipWithM_)
import Data.Conduit.List
import Test.Hspec
import Test.QuickCheck
import Data.Conduit.Tar
import System.Directory
import Data.ByteString as S
import Data.ByteString.Char8 as S8
import Data.Int
import System.IO
import System.FilePath
import Control.Exception

main :: IO ()
main = do
    let baseTmp = "tar-conduit-tests"
    isStack <- doesDirectoryExist ".stack-work"
    let testPaths =
            ["src", "./tests", "README.md", "ChangeLog.md", "LICENSE"] ++
            if isStack
                then [".stack-work", "./sample"]
                else []
    hspec $ do
        describe "tar/untar" $ do
            let tarUntarContent dir =
                    runConduitRes $
                    yield dir .| void tarFilePath .| untar (const (foldC >>= yield)) .| foldC
            it "content" $ do
                c <- collectContent "src"
                tarUntarContent "src" `shouldReturn` c
        describe "tar/untar/tar" $ do
            around (withTempTarFiles baseTmp) $
                it "structure" $ \(fpIn, hIn, outDir, fpOut) -> do
                    writeTarball hIn testPaths
                    hClose hIn
                    extractTarball fpIn (Just outDir)
                    curDir <- getCurrentDirectory
                    finally
                        (setCurrentDirectory outDir >> createTarball fpOut testPaths)
                        (setCurrentDirectory curDir)
                    tb1 <- readTarball fpIn
                    tb2 <- readTarball fpOut
                    P.length tb1 `shouldBe` P.length tb2
                    zipWithM_ shouldBe (fmap fst tb2) (fmap fst tb1)
                    zipWithM_ shouldBe (fmap snd tb2) (fmap snd tb1)
        describe "ustar" ustarSpec
        describe "GNUtar" gnutarSpec

defFileInfo :: FileInfo
defFileInfo =
    FileInfo
    { filePath = "test-file-name"
    , fileUserId = 1000
    , fileUserName = "test-user-name"
    , fileGroupId = 1000
    , fileGroupName = "test-group-name"
    , fileMode = 0o644
    , fileSize = 0
    , fileType = FTNormal
    , fileModTime = 123456789
    }


fileInfoExpectation :: [(FileInfo, ByteString)] -> IO ()
fileInfoExpectation files = do
    let source = P.concat [[Left fi, Right content] | (fi, content) <- files]
        collectBack fi = do
            content <- foldC
            yield (fi, content)
    result <- runConduit $ sourceList source .| void tar .| untar collectBack .| sinkList
    result `shouldBe` files

data GnuTarFile = GnuTarFile FileInfo (Maybe ByteString) deriving (Show, Eq)

asciiGen :: Int -> Gen ByteString
asciiGen n = S.pack <$> vectorOf n (frequency [(1, pure 0x2f), (20, choose (0x20, 0x7e))])

instance Arbitrary GnuTarFile where
    arbitrary = do
        filePathLen <- (`mod` 6) <$> arbitrary
        filePath <- asciiGen filePathLen
        NonNegative fileUserId64 <- arbitrary
        let fileUserId = fromIntegral (fileUserId64 :: Int64)
        NonNegative fileGroupId64 <- arbitrary
        let fileGroupId = fromIntegral (fileGroupId64 :: Int64)
        fileUserNameLen <- (`mod` 32) <$> arbitrary
        fileUserName <- asciiGen fileUserNameLen
        fileGroupNameLen <- (`mod` 32) <$> arbitrary
        fileGroupName <- asciiGen fileGroupNameLen
        fileMode <- fromIntegral <$> choose (0o000 :: Word, 0o777)
        fileType <- oneof [pure FTNormal, pure FTDirectory, FTSymbolicLink <$> asciiGen filePathLen]
        (fileSize, mContent) <- case fileType of
            FTNormal -> do
                content <- S.pack <$> arbitrary
                return (fromIntegral (S.length content), Just content)
            _ -> return (0, Nothing)
        fileModTime <- fromIntegral <$> (arbitrary :: Gen Int64)
        return (GnuTarFile FileInfo {..} mContent)


_fileInfoProperty :: [GnuTarFile] -> Property
_fileInfoProperty files =
    Just source === do
        let collectBack fi = do
                yield $ Left fi
                case fileType fi of
                    FTNormal -> do
                        content <- foldC
                        yield $ Right content
                    _ -> return ()
        runConduit $ sourceList source .| void tar .| untar collectBack .| sinkList
  where
    source =
        P.concat [Left fi : maybe [] ((: []) . Right) mContent | GnuTarFile fi mContent <- files]


emptyFileInfoExpectation :: FileInfo -> IO ()
emptyFileInfoExpectation fi = fileInfoExpectation [(fi, "")]

ustarSpec :: Spec
ustarSpec = do
    it "minimal" $ do
        emptyFileInfoExpectation defFileInfo
    it "long file name <255" $ do
        emptyFileInfoExpectation $
            defFileInfo {filePath = S8.pack (P.replicate 99 'f' </> P.replicate 99 'o')}


gnutarSpec :: Spec
gnutarSpec = do
    it "LongLink - a file with long file name" $ do
        emptyFileInfoExpectation $
            defFileInfo
            { filePath =
                  S8.pack (P.replicate 100 'f' </> P.replicate 100 'o' </> P.replicate 99 'b')
            }
    it "LongLink - multiple files with long file names" $ do
        fileInfoExpectation
            [ ( defFileInfo
                { filePath =
                      S8.pack (P.replicate 100 'f' </> P.replicate 100 'o' </> P.replicate 99 'b')
                , fileSize = 10
                }
              , "1234567890")
            , ( defFileInfo
                { filePath =
                      S8.pack (P.replicate 1000 'g' </> P.replicate 1000 'o' </> P.replicate 99 'b')
                , fileSize = 11
                }
              , "abcxdefghij")
            ]
    it "Large User Id" $ do emptyFileInfoExpectation $ defFileInfo {fileUserId = 0o777777777}
    it "All Large Numeric Values" $ do
        emptyFileInfoExpectation $
            defFileInfo
            { fileUserId = 0x7FFFFFFFFFFFFFFF
            , fileGroupId = 0x7FFFFFFFFFFFFFFF
            , fileModTime = fromIntegral (maxBound :: Int64)
            }
    it "Negative Values" $ do
        emptyFileInfoExpectation $ defFileInfo {fileModTime = fromIntegral (minBound :: Int64)}
        emptyFileInfoExpectation $ defFileInfo {fileModTime = -10}
        emptyFileInfoExpectation $ defFileInfo {fileUserId = fromIntegral (minBound :: Int64)}
    it "Negative Size" $
        (emptyFileInfoExpectation (defFileInfo {fileSize = -10}) `shouldThrow`
         (\case
              TarCreationError _ -> True
              _ -> False))
    --it "tar/untar Property" $ property fileInfoProperty


withTempTarFiles :: FilePath -> ((FilePath, Handle, FilePath, FilePath) -> IO c) -> IO c
withTempTarFiles base =
    bracket
        (do tmpDir <- getTemporaryDirectory
            (fp1, h1) <- openBinaryTempFile tmpDir (addExtension base ".tar")
            let outPath = dropExtension fp1 ++ ".out"
            return (fp1, h1, outPath, addExtension outPath ".tar")
        )
        (\(fp, h, dirOut, fpOut) -> do
             hClose h
             removeFile fp
             doesDirectoryExist dirOut >>= (`when` removeDirectoryRecursive dirOut)
             doesFileExist fpOut >>= (`when` removeFile fpOut)
        )


readTarball
  :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
     FilePath -> m [(FileInfo, Maybe ByteString)]
readTarball fp = runConduitRes $ sourceFileBS fp .| untar grabBoth .| sinkList
  where
    grabBoth fi =
        case fileType fi of
            FTNormal -> do
                content <- foldC
                yield (fi, Just content)
            _ -> yield (fi, Nothing)


collectContent :: FilePath -> IO (ByteString)
collectContent dir =
    runConduitRes $
    sourceDirectoryDeep False dir .| mapMC (\fp -> runConduit (sourceFileBS fp .| foldC)) .| foldC

