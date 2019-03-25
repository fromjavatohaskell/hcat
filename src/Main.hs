-- implementation of linux cat command line utility

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Control.Monad                  ( when )
import           Data.Maybe                     ( listToMaybe )
import           Foreign.Ptr                    ( Ptr )
import           Foreign.Ptr                    ( plusPtr )
import           GHC.IO.Buffer                  ( newByteBuffer )
import           GHC.IO.Buffer                  ( withBuffer )
import           GHC.IO.Buffer                  ( BufferState(WriteBuffer) )
import           GHC.Word                       ( Word8 )
import           System.Environment             ( getArgs )
import           System.Posix.Fcntl             ( fileAdvise )
import           System.Posix.Fcntl             ( Advice(AdviceSequential) )
import           System.Posix.IO                ( fdWriteBuf )
import           System.Posix.IO                ( fdReadBuf )
import           System.Posix.IO                ( stdInput )
import           System.Posix.IO                ( stdOutput )
import           System.Posix.IO                ( openFd )
import           System.Posix.IO                ( OpenMode(ReadOnly) )
import           System.Posix.IO                ( defaultFileFlags )
import           System.Posix.Types             ( Fd )
import           System.Posix.Types             ( ByteCount )

chunkSize :: ByteCount
chunkSize = 128 * 1024

-- there could be partial write
-- so we invoke write for the rest of the buffer
-- due to some reason partial write happens when haskell program compiled without thread support
-- and output is pipe
writeWholeBuffer :: Fd -> Ptr Word8 -> ByteCount -> IO ()
writeWholeBuffer fd buffer bufferLength = writeWholeBuffer' 0
 where
  writeWholeBuffer' offset = do
    bufferLength' <- return $! bufferLength - offset
    buffer'       <- return $! plusPtr buffer $ fromIntegral offset
    writeLength   <- fdWriteBuf fd buffer' bufferLength'
    when (writeLength == 0) $ error "unable to write to output"
    when (writeLength < bufferLength') $ do
      offset' <- return $! offset + writeLength
      writeWholeBuffer' offset'

encodeStream :: Fd -> Fd -> Ptr Word8 -> IO ()
encodeStream fdIn fdOut buffer = encodeStream'
 where
  encodeStream' = do
    readLength <- fdReadBuf fdIn buffer chunkSize
    when (readLength > 0) $ do
      writeWholeBuffer fdOut buffer readLength
      encodeStream'

main :: IO ()
main = do
  fdIn <- fmap listToMaybe getArgs >>= getHandle
  fileAdvise fdIn 0 0 AdviceSequential
  buffer <- newByteBuffer (fromIntegral chunkSize) WriteBuffer
  withBuffer buffer $ encodeStream fdIn stdOutput
 where
  getHandle (Just filename) =
    openFd filename ReadOnly Nothing defaultFileFlags
  getHandle Nothing = return stdInput

