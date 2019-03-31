-- modified sourceHandle function from conduit library
-- https://hackage.haskell.org/package/conduit

-- default chunk size increased to 128K to decrease number of system calls
-- and improve overall performance of application

module ConduitUtil
  (sourceHandle)
where

import Conduit hiding (sourceHandle)
import System.IO (Handle)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S


defaultChunkSize :: Int
defaultChunkSize = 128 * 1024


sourceHandle :: MonadIO m
             => Handle
             -> ConduitT i ByteString m ()
sourceHandle h =
    loop
  where
    loop = do
        bs <- liftIO (S.hGetSome h defaultChunkSize)
        if S.null bs
            then return ()
            else yield bs >> loop


