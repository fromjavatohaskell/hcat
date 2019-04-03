-- implementation of linux cat command line utility

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Conduit
import           Data.Conduit.Binary            ( sourceHandleRangeWithBuffer )
import qualified System.IO as IO
import           Data.Maybe                     ( listToMaybe )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  h <- fmap listToMaybe getArgs >>= getHandle
  runConduitRes $ sourceHandleRangeWithBuffer h Nothing Nothing 131072 .| sinkHandle IO.stdout
 where
  getHandle (Just filename) = IO.openFile filename IO.ReadMode
  getHandle Nothing = return IO.stdin

