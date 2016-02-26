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
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as L
import           Pipes
import qualified Pipes.Prelude          as P

process :: Monad m => (L.ByteString -> L.ByteString) -> Pipe ByteString ByteString m ()
process f = P.map (L.toStrict . f . L.fromStrict)

decompress :: Monad m => Pipe ByteString ByteString m ()
decompress = process BZip.decompress

compress :: Monad m => Pipe ByteString ByteString m ()
compress = process BZip.compress

decompressWith :: Monad m => DecompressParams -> Pipe ByteString ByteString m ()
decompressWith p = process (BZip.decompressWith p)

compressWith :: Monad m => CompressParams -> Pipe ByteString ByteString m ()
compressWith p = process (BZip.compressWith p)
