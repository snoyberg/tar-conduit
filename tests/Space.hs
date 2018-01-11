{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Measure space usage by the tar algo.

module Main where

import           Conduit
import           Control.DeepSeq
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Tar as Tar
import           Data.Conduit.Tar.Types
import           Data.Monoid
import           GHC.Generics
import           System.Posix.Types
import           Weigh

main :: IO ()
main = do
  mainWith
    (do setColumns [Case, Allocated, Max, Live, GCs, Check]
        sequence_
          [ validateAction
            ("tar " ++ show count ++ " files")
            (const
               (runConduitRes
                  (CL.sourceList files .| void Tar.tar .| CL.sinkNull)))
            ()
            (\w -> do
               guard (weightMaxBytes w > 50000)
               pure "Exceeded maximum bytes!")
          | count :: Int <- [1, 10, 100, 1000, 10000]
          , let !files =
                  force
                    (concat
                       (map
                          (\i -> makeFileN (S8.pack (show i) <> ".txt") 10)
                          [1 :: Int .. count]))
          ]
        sequence_
          [ validateAction
            ("tar file of " ++ show bytes ++ " bytes")
            (const
               (runConduitRes
                  (CL.sourceList files .| void Tar.tar .| CL.sinkNull)))
            ()
            (\w -> do
               guard
                 (weightMaxBytes w > 50000 || weightAllocatedBytes w > 70000)
               pure "Exceeded maximum bytes or allocated bytes!")
          | bytes :: Int <- [1, 10, 100, 1000, 10000]
          , let !files = force (makeFileN "file.txt" bytes)
          ]
        sequence_
          [ validateAction
            ("untar " ++ show count ++ " files")
            (const
               (runConduitRes
                  (CL.sourceList files .| void Tar.tar .|
                   void (Tar.untar (const (pure ()))) .|
                   CL.sinkNull)))
            ()
            (const Nothing)
          | count :: Int <- [1, 10, 100, 1000, 10000]
          , let !files =
                  force
                    (concat
                       (map
                          (\i -> makeFileN (S8.pack (show i) <> ".txt") 10)
                          [1 :: Int .. count]))
          ]
        sequence_
          [ validateAction
            ("untar file of " ++ show bytes ++ " bytes")
            (const
               (runConduitRes
                  (CL.sourceList files .| void Tar.tar .|
                   void (Tar.untar (const (pure ()))) .|
                   CL.sinkNull)))
            ()
            (\w -> do
               guard
                 (weightMaxBytes w > 11000 || weightAllocatedBytes w > 61000)
               pure "Exceeded maximum bytes or allocated bytes!")
          | bytes :: Int <- [1, 10, 100, 1000, 10000]
          , let !files = force (makeFileN "file.txt" bytes)
          ])

----------------------------------------------------------------------
-- Helpers

makeFileN :: ByteString -> Int -> [Either FileInfo ByteString]
makeFileN fname bytes =
  let contents = S8.pack (take bytes (cycle "Hello Dave"))
  in [ Left
         FileInfo
         { filePath = fname
         , fileUserId = 0
         , fileUserName = "test"
         , fileGroupId = 0
         , fileGroupName = "test"
         , fileMode = 0
         , fileSize = fromIntegral (S.length contents)
         , fileType = FTNormal
         , fileModTime = 1234
         }
     , Right contents
     ]

----------------------------------------------------------------------
-- NFData helper instances. If ever these instances become available,
-- these can just be removed.

deriving instance Generic FileInfo
instance NFData FileInfo

deriving instance Generic FileType
instance NFData FileType

deriving instance Generic CUid
instance NFData CUid

deriving instance Generic COff
instance NFData COff

deriving instance Generic CGid
instance NFData CGid

deriving instance Generic CMode
instance NFData CMode
