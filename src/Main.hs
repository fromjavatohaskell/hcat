-- implementation of linux cat command line utility

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           GHC.Word                       ( Word8(..) )
import           Data.Maybe                     ( listToMaybe )
import           Control.Monad                  ( when )
import qualified System.Environment            as E
import qualified GHC.IO.Buffer                 as Buf
import           Foreign.Ptr                    ( Ptr )
import           Foreign.Ptr                    ( plusPtr )
import qualified System.Posix.IO               as IO
import           System.Posix.Types             ( Fd(..) )
import           System.Posix.Types             ( ByteCount )
import           System.Posix.Fcntl             ( fileAdvise )
import           System.Posix.Fcntl             ( Advice(..) )

chunkSize :: ByteCount
chunkSize = 128 * 1024

-- there could be partial write
-- so we invoke write for the rest of the buffer
-- due to some reason it happens when haskell program compiled without thread support
writeWholeBuffer :: Fd -> Ptr Word8 -> ByteCount -> IO ()
writeWholeBuffer fd buffer bufferLength = do
  writeLength <- IO.fdWriteBuf fd buffer bufferLength
  when (writeLength == 0) $ error "unable to write to output"
  when (writeLength < bufferLength) $ do
    let buffer' = plusPtr buffer $ fromIntegral writeLength
    let bufferLength' = bufferLength - writeLength
    writeWholeBuffer fd buffer' bufferLength'

encodeStream :: Fd -> Fd -> Ptr Word8 -> IO ()
encodeStream fdIn fdOut buffer = do
  readLength <- IO.fdReadBuf fdIn buffer chunkSize
  when (readLength > 0) $ do
    writeWholeBuffer fdOut buffer readLength
    encodeStream fdIn fdOut buffer

main :: IO ()
main = do
  fdIn <- fmap listToMaybe E.getArgs >>= getHandle
  fileAdvise fdIn 0 0 AdviceSequential
  buffer <- Buf.newByteBuffer (fromIntegral chunkSize) Buf.WriteBuffer
  Buf.withBuffer buffer $ encodeStream fdIn IO.stdOutput
 where
  getHandle (Just filename) =
    IO.openFd filename IO.ReadOnly Nothing IO.defaultFileFlags
  getHandle Nothing = return IO.stdInput

