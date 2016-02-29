{-# LANGUAGE ViewPatterns #-}
import qualified Codec.Compression.BZip     as Z
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy       as LZ
import qualified Data.ByteString.Lazy.Char8 as L
import           Pipes
import qualified Pipes.ByteString           as PB
import           Pipes.BZip
import           Pipes.Safe
import           System.Directory           (removeFile)
import qualified System.IO                  as IO

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Property

import           Prelude                    as P

safeReadFile :: FilePath -> Producer S.ByteString (SafeT IO) ()
safeReadFile file = bracket
    (IO.openFile file IO.ReadMode)
    IO.hClose
    PB.fromHandle

safeWriteFile :: FilePath -> Consumer S.ByteString (SafeT IO) ()
safeWriteFile file = bracket
    (IO.openFile file IO.WriteMode)
    IO.hClose
    PB.toHandle

main :: IO ()
main = hspec $ do
    describe "decompress" $ do
        forM_ ["sample1", "sample2", "sample3" ] $ \file -> do
            it ("correctly " ++ file ++ ".bz2") $ do
                runSafeT $ runEffect $ bunzip2 (safeReadFile ("test/" ++ file ++ ".bz2")) >-> safeWriteFile ("test/" ++ file ++ ".check")
                dec <- L.take (10^9) <$> L.readFile ("test/" ++ file ++ ".check")
                ref <- L.readFile ("test/" ++ file ++ ".ref")
                removeFile ("test/" ++ file ++ ".check")
                dec `shouldBe` ref
    describe "compress" $ do
        forM_ ["sample1", "sample2", "sample3" ] $ \file -> do
            it ("correctly " ++ file ++ ".ref")  $ do
                runSafeT $ runEffect $ bzip2 (safeReadFile ("test/" ++ file ++ ".ref")) >-> safeWriteFile ("test/" ++ file ++ ".check2")
                bz <- L.readFile ("test/" ++ file ++ ".check2")
                ref <- L.readFile ("test/" ++ file ++ ".9.bz2")
                removeFile ("test/" ++ file ++ ".check2")
                bz `shouldBe` ref
    describe "lazy ByteString" $ do
        it "correctly 100000" $ do
            lazyCheck 100000 `shouldReturn` ()
            removeFile "test/result.dat"
        it "correctly 1000000" $ do
            lazyCheck 1000000`shouldReturn` ()
            removeFile "test/result.dat"
        it "correctly 10000000" $ do
            lazyCheck 10000000`shouldReturn` ()
            removeFile "test/result.dat"

lazyCheck :: Int -> IO ()
lazyCheck i = do
    theData <- randomData i
    let compressedData = Z.compress theData
        pipesDecompressedData = runSafeT $ PB.toLazyM $ bunzip2 (PB.fromLazy compressedData)
    LZ.writeFile "test/result.dat" =<< pipesDecompressedData


randomData :: Int -> IO L.ByteString
randomData i = L.pack . take i <$> getRandoms


