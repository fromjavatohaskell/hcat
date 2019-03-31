-- implementation of linux cat command line utility

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Conduit hiding (sourceHandle)
import qualified System.IO as IO
import           Data.Maybe                     ( listToMaybe )
import           System.Environment             ( getArgs )
import qualified Data.ByteString as S


defaultChunkSize :: Int
defaultChunkSize = 128 * 1024


sourceHandle :: MonadIO m
             => IO.Handle
             -> ConduitT i S.ByteString m ()
sourceHandle h =
    loop
  where
    loop = do
        bs <- liftIO (S.hGetSome h defaultChunkSize)
        if S.null bs
            then return ()
            else yield bs >> loop


main :: IO ()
main = do
  h <- fmap listToMaybe getArgs >>= getHandle
  runConduitRes $ sourceHandle h .| sinkHandle IO.stdout
 where
  getHandle (Just filename) = IO.openFile filename IO.ReadMode
  getHandle Nothing = return IO.stdin

