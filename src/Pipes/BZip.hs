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

process
  :: Monad m
  => (L.ByteString -> L.ByteString)
  -> Pipe ByteString ByteString m ()
process f = forever $ do
  bs <- await
  let d = f (L.fromStrict bs)
  yield $ L.toStrict d

decompress :: Monad m => Pipe ByteString ByteString m ()
decompress = process BZip.decompress

compress :: Monad m => Pipe ByteString ByteString m ()
compress = process BZip.compress

decompressWith :: Monad m => DecompressParams -> Pipe ByteString ByteString m ()
decompressWith p = process (BZip.decompressWith p)

compressWith :: Monad m => CompressParams -> Pipe ByteString ByteString m ()
compressWith p = process (BZip.compressWith p)
