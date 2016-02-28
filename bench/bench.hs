import System.Environment
import Pipes
import Pipes.BZip
import Pipes.Safe
import qualified Pipes.ByteString as PB
import Control.Monad (forever)
import qualified System.IO as IO

safeReadFile :: FilePath -> Producer PB.ByteString (SafeT IO) ()
safeReadFile file = bracket
    (IO.openFile file IO.ReadMode)
    IO.hClose 
    PB.fromHandle

main :: IO ()
main = do
  [file] <- getArgs
  runSafeT $ runEffect $ bzip2 (safeReadFile file) >-> (forever $ void await)
