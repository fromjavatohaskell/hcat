-- implementation of linux cat command line utility

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Conduit hiding (sourceHandle)
import           ConduitUtil (sourceHandle)
import qualified System.IO as IO
import           Data.Maybe                     ( listToMaybe )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  h <- fmap listToMaybe getArgs >>= getHandle
  runConduitRes $ sourceHandle h .| sinkHandle IO.stdout
 where
  getHandle (Just filename) = IO.openFile filename IO.ReadMode
  getHandle Nothing = return IO.stdin

