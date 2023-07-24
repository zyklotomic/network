module Network.Socket.URing where

import System.Linux.IO.URing.Sqe
import System.Posix.Types

import Foreign.Ptr
import Foreign.C.Types
import Data.Word

import Network.Socket.Internal (throwSocketError)
import Data.Int (Int32)

#include <linux/io_uring.h>

throwSocketErrorIfCqeResNegative
  :: (Show a, Ord a, Num a)
  => String
  -> IO a
  -> IO a
throwSocketErrorIfCqeResNegative opcodeMsg cqeRes = do
  res <- cqeRes
  if (res < 0)
    then do
      let errno = -res
          errMsg = opcodeMsg ++ " -- errno: " ++ show errno
      throwSocketError errMsg
    else return res  

setFdBufLenFlags
  :: CInt      -- ^ 'Fd' to set
  -> Ptr Word8 -- ^ buffer
  -> CSize     -- ^ length in bytes
  -> CInt      -- ^ flags
  -> SqeBuilder ()
setFdBufLenFlags fd buf len flags = do
  setFd fd'
  setAddr buf
  setLen len'
  setFlags flags'
  where
    fd'    = fromIntegral fd
    len'   = fromIntegral len
    flags' = fromIntegral flags
  

-- | Prepare send SQE
send
  :: CInt      -- ^ 'Fd' to send to
  -> Ptr Word8 -- ^ source buffer
  -> CSize     -- ^ length in bytes
  -> CInt      -- ^ flags
  -> UserData
  -> SqeBuilder () 
send fd buf len flags userd = do
  zeroIt
  setOpCode (#const IORING_OP_SEND)
  setFdBufLenFlags fd buf len flags
  setUserData userd

-- | Prepare recv SQE
recv
  :: CInt      -- ^ 'Fd' to send to
  -> Ptr Word8 -- ^ source buffer
  -> CSize     -- ^ length in bytes
  -> CInt      -- ^ flags
  -> UserData
  -> SqeBuilder ()
recv fd buf len flags userd = do
  zeroIt
  setOpCode (#const IORING_OP_RECV)
  setFdBufLenFlags fd buf len flags
  setUserData userd

