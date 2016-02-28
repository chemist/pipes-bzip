{-# LANGUAGE ViewPatterns #-}
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Pipes.Safe
import Pipes.BZip
import Pipes
import qualified Pipes.ByteString as PB
import qualified System.IO as IO

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

import Prelude as P

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
                dec `shouldBe` ref
                {--
  describe "compress" $ do
    prop ". decompress == id" $ \((`mod` (2^16)) . abs -> n) -> do
      morallyDubiousIOProperty $ do
        let bsize = 8192
        ss <- P.takeWhile (not. null)
              . P.map (P.take bsize)
              . P.iterate (P.drop bsize)
              <$> Control.Monad.replicateM (abs n) randomIO
        dest <- runResourceT $ do
          C.sourceList (P.map S.pack ss) =$= bzip2 =$= bunzip2 $$ B.take (10^9)
        return $ dest == L.pack (P.concat ss)
        --}
