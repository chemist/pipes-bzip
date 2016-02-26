module Pipes.BZip
    (
      module Codec.Compression.BZip
    , compress
    , decompress
    , compressWith
    , decompressWith
    ) where

import           Codec.Compression.BZip (CompressParams (..),
                                         DecompressParams (..),
                                         defaultCompressParams,
                                         defaultDecompressParams)
import qualified Codec.Compression.BZip as BZip
import           Control.Monad          (forever)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import           Pipes
import           Pipes.ByteString

someFunc :: IO ()
someFunc = putStrLn "someFunc"

decompress :: Monad m => Pipe ByteString ByteString m ()
decompress = forever $ do
    bs <- await
    let d = BZip.decompress (L.fromStrict bs)
    yield $ L.toStrict d

compress :: Monad m => Pipe ByteString ByteString m ()
compress = forever $ do
    bs <- await
    let d = BZip.compress (L.fromStrict bs)
    yield $ L.toStrict d

decompressWith :: Monad m => DecompressParams -> Pipe ByteString ByteString m ()
decompressWith p = forever $ do
    bs <- await
    let d = BZip.decompressWith p (L.fromStrict bs)
    yield $ L.toStrict d

compressWith :: Monad m => CompressParams -> Pipe ByteString ByteString m ()
compressWith p = forever $ do
    bs <- await
    let d = BZip.compressWith p (L.fromStrict bs)
    yield $ L.toStrict d
