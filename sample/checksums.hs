#!/usr/bin/env stack
-- stack --resolver lts-7.5 exec --package tar-conduit --package conduit-extra --package cryptonite-conduit -- ghc -O2 -threaded
import qualified Crypto.Hash.Conduit as CH
import qualified Data.Conduit.Tar    as CT

import Conduit
import Crypto.Hash (Digest, SHA256)
import Control.Monad (when)
import Data.Conduit.Zlib (ungzip)
import Data.ByteString (ByteString)
import System.Environment

filedigests :: FilePath -> IO ()
filedigests fp = runConduitRes (  sourceFileBS fp          -- read the raw file
                               .| ungzip                   -- gunzip
                               .| CT.untar                 -- decode the tar archive
                               .| CT.withEntries hashentry -- process each file
                               .| printC                   -- print the results
                               )
    where
        hashentry :: Monad m => CT.Header -> Conduit ByteString m (FilePath, Digest SHA256)
        hashentry hdr = when (CT.headerFileType hdr == CT.FTNormal) $ do
            hash <- CH.sinkHash
            yield (CT.headerFilePath hdr, hash)

main :: IO ()
main = do
    [fp] <- getArgs
    filedigests fp
