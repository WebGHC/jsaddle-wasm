{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}


module Language.Javascript.JSaddle.Wasm (
  run
  ) where

import Control.Monad
import Control.Concurrent (killThread, forkIO, threadDelay, yield)
import Control.Concurrent.MVar
import Control.Exception (try, AsyncException, IOException, throwIO, fromException, finally)
import Data.Aeson (encode, decode)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BSIO
import Data.Int (Int64)
import Data.Word (Word32)

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Marshal.Alloc (mallocBytes, free, malloc)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable

import System.IO (openBinaryFile, IOMode(..))

import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Run (runJavaScript)

-- JSaddle wasm HS side interface for running on main thread

-- C side does not return
foreign import capi "jsaddle-wasm-interface.h jsaddle_wasm_init" cJsaddleWasmInit :: StablePtr HsEnv -> IO ()

run :: Int -> JSM () -> IO ()
run _ entryPoint = cJsaddleWasmInit =<< jsaddleInit entryPoint


-- This contains everything needed to do a re-entry from foriegn export API
data HsEnv = HsEnv
  { _hsEnv_outgoingMessages :: MVar [TryReq]
  , _hsEnv_processRsp :: ([Rsp] -> IO ())
  , _hsEnv_processSyncCommand :: (SyncCommand -> IO [(Int, SyncBlockReq)])
  , _hsEnv_msgBufferPtr :: Ptr SharedMsgBuffer
  }

-- Maintain a buffer for communication and its size
data SharedMsgBuffer = SharedMsgBuffer CString Word32

-- Exports
foreign export ccall hsJsaddleProcessResult :: StablePtr HsEnv -> Bool -> Int -> IO Int64

foreign export ccall hsJsaddleBufferMalloc :: StablePtr HsEnv -> Word32 -> IO (Ptr SharedMsgBuffer)

hsJsaddleBufferMalloc :: StablePtr HsEnv -> Word32 -> IO (Ptr SharedMsgBuffer)
hsJsaddleBufferMalloc envPtr newSize = do
  HsEnv _ _ _ bufPtr <- deRefStablePtr envPtr
  SharedMsgBuffer buf size <- peek bufPtr
  when (newSize > size) $ do
    free buf
    newBuf <- mallocBytes (fromIntegral newSize)
    poke bufPtr (SharedMsgBuffer newBuf newSize)
  pure bufPtr

jsaddleInit :: JSM () -> IO (StablePtr HsEnv)
jsaddleInit entryPoint = do
  putStrLn "Starting JSaddle-Wasm"
  outgoingMessages <- newMVar []
  lockInit <- newEmptyMVar

  let
    sendBatch :: [TryReq] -> IO ()
    sendBatch b = do
      modifyMVar_ outgoingMessages $ \a -> pure (a ++ b)
      tryPutMVar lockInit ()
      pure ()

  (processRsp, processSyncCommand, env) <- runJavaScript sendBatch

  putStrLn "runJavaScript done"

  forkIO $ runJSM entryPoint env

  -- This lock ensures that we have started the HS side threads
  -- TODO: Is this really needed?
  takeMVar lockInit

  forkIO $ forever $ do
    threadDelay (5*1000*1000)
    putStrLn "JSaddle-Wasm heartbeat"

  bufPtr <- do
    let initSize = (1024 * 1024) :: Word32
    buf <- mallocBytes (fromIntegral initSize)
    ptr <- malloc
    poke ptr $ SharedMsgBuffer buf initSize
    pure ptr

  newStablePtr $ HsEnv outgoingMessages processRsp processSyncCommand bufPtr

-- hsJsaddleProcessResult reads data from dataPtr, and writes back to the same place
hsJsaddleProcessResult :: StablePtr HsEnv -> Bool -> Int -> IO Int64
hsJsaddleProcessResult envPtr isSync dataLen = do
  -- putStrLn "Doing hsJsaddleProcessResult"
  HsEnv outgoingMessages processRsp processSyncCommand bufPtr <- deRefStablePtr envPtr
  SharedMsgBuffer dataPtr bufSize <- peek bufPtr

  mBs <- if (dataLen == 0)
    then pure Nothing
    else Just <$> BS8.packCStringLen (dataPtr, dataLen)

  mReqs <- forM mBs $ \bs -> if isSync
    then do
      case decode (BS.fromStrict bs) of
        Nothing -> error "Decode error: SyncCommand"
        Just command -> Just <$> processSyncCommand command
    else do
      case decode (BS.fromStrict bs) of
        Nothing -> error "Decode error: [Rsp]"
        Just rsps -> Nothing <$ processRsp rsps

  let
    sendData m = do
      let outData = encode m
          dataLen = fromIntegral $ BS.length outData
      targetPtr <- if (dataLen > bufSize)
        then do
          let newSize = head $ dropWhile (< dataLen) $ map ((^) 2) [1..]
          hsJsaddleBufferMalloc envPtr newSize
          SharedMsgBuffer newBuf _ <- peek bufPtr -- bufPtr remains same
          pure newBuf
        else (pure dataPtr)
      -- putStrLn $ "outmsgsize: " <> show dataLen
      BSIO.useAsCStringLen (BS.toStrict outData) $ \(ptr, len) -> copyBytes targetPtr ptr len
      pure $ BS.length outData

  case join mReqs of
    Just reqs -> sendData reqs
    Nothing -> do
      -- Give the forked threads some time to work
      let
        loop m1 n = do
          threadDelay (1000*1)
          m2 <- modifyMVar outgoingMessages $ \m -> pure ([], m)
          if (null m2) || (n < 0)
            then pure m1
            else loop (m1 ++ m2) (n - 1)
      reqs <- loop [] 10
      case reqs of
        [] -> pure 0
        _ -> sendData reqs

-- The Char* and size values are read by JS code
-- So this should be in sync with JS
instance Storable SharedMsgBuffer where
  sizeOf _ = sizeOf (undefined :: CString) + sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: CString)
  peek p = SharedMsgBuffer
    <$> peekByteOff p 0
    <*> peekByteOff p (sizeOf (undefined :: CString))
  poke p (SharedMsgBuffer c s) = do
    pokeByteOff p 0 c
    pokeByteOff p (sizeOf (undefined :: CString)) s
