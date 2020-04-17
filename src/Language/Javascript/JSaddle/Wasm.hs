{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}

module Language.Javascript.JSaddle.Wasm (
  run
  , jsaddleInit
  , hsJsaddleProcessResult
  , HsEnv
  ) where

import Control.Monad (when, void, forever)
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

import Foreign.StablePtr
import Foreign.C.String
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)

import System.IO (openBinaryFile, IOMode(..))

import Language.Javascript.JSaddle.Types (JSM, Batch, Results)
import Language.Javascript.JSaddle.Run (syncPoint, runJavaScript)

run :: Int -> JSM () -> IO ()
run _ entryPoint = do
  putStrLn "Starting JSaddle-Wasm"

  jsInOut <- openBinaryFile "/dev/jsaddle_inout" ReadWriteMode

  let
    sendBatch :: Batch -> IO ()
    sendBatch b = do
      let payload = encode b
          msg = (Binary.encode (fromIntegral $ BS.length payload :: Word32)) <> payload
      BS.hPut jsInOut msg

    receiveDataMessage :: IO (ByteString)
    receiveDataMessage = loop
      where
        loop = do
          threadDelay 1
          try (BSIO.hGetNonBlocking jsInOut 4)
            >>= \case
              (Left (ex :: IOException)) -> loop
              (Right v)
                | BSIO.null v -> loop
                | otherwise -> do
                  -- Somehow we get this size in reverse!!
                  let size = Binary.decode (BS.reverse $ BS.fromStrict v) :: Word32
                  BS.fromStrict <$> BSIO.hGetNonBlocking jsInOut (fromIntegral size)

    -- When to exit? never?
    waitTillClosed = forever $ do
      threadDelay (1*1000*1000)
      putStrLn "JSaddle-Wasm heartbeat"
      waitTillClosed

  (processResult, _, start) <-
    runJavaScript sendBatch entryPoint
  forkIO . forever $ do
    msgs <- receiveDataMessage
    processIncomingMsgs processResult msgs

  start
  waitTillClosed

processIncomingMsgs :: (Results -> IO ()) -> ByteString -> IO ()
processIncomingMsgs cont msgs = if (BS.length msgs < 5)
  then error $ "processIncomingMsgs: no more data while looping: " <> show msgs
  else do
    let
      size = Binary.decode (BS.take 4 msgs) :: Word32
      (thisMsg, rest) = BS.splitAt (fromIntegral $ 4 + size) msgs
    case decode (BS.drop 4 thisMsg) of
      Nothing -> error $ "jsaddle Results decode failed : " <> show thisMsg
      Just r  -> cont r
    case BS.length rest of
      0 -> return ()
      _ -> processIncomingMsgs cont rest

-- JSaddle wasm HS side interface for running on main thread

-- This contains everything needed to do a re-entry from foriegn export API
data HsEnv = HsEnv
  { _hsEnv_outgoingMessages :: MVar [Batch]
  , _hsEnv_processResult :: (Results -> IO ())
  , _hsEnv_processSyncResult :: (Results -> IO (Batch))
  }

-- Exports
foreign export ccall hsJsaddleProcessResult :: StablePtr HsEnv -> CString -> Bool -> Int -> IO Int64

foreign export ccall hsMalloc :: Int -> IO CString

hsMalloc :: Int -> IO CString
hsMalloc = mallocBytes

jsaddleInit :: Int -> JSM () -> IO (StablePtr HsEnv)
jsaddleInit _ entryPoint = do
  putStrLn "Starting JSaddle-Wasm"
  outgoingMessages <- newMVar []
  lockInit <- newEmptyMVar

  let
    sendBatch :: Batch -> IO ()
    sendBatch b = do
      modifyMVar_ outgoingMessages $ \a -> pure (a ++ [b])
      tryPutMVar lockInit ()
      pure ()

  (processResult, processSyncResult, start) <-
    runJavaScript sendBatch entryPoint
  putStrLn "runJavaScript done"

  forkIO $ start

  -- This lock ensures that we have started the HS side threads
  -- TODO: Is this really needed?
  takeMVar lockInit

  forkIO $ forever $ do
    threadDelay (5*1000*1000)
    putStrLn "JSaddle-Wasm heartbeat"

  newStablePtr $ HsEnv outgoingMessages processResult processSyncResult

-- hsJsaddleProcessResult reads data from dataPtr, and writes back to the same place
-- TODO: the size of dataPtr memory should be adjusted for larger data
hsJsaddleProcessResult :: StablePtr HsEnv -> CString -> Bool -> Int -> IO Int64
hsJsaddleProcessResult envPtr dataPtr isSync dataLen = do
  -- putStrLn "Doing hsJsaddleProcessResult"
  HsEnv outgoingMessages processResult processSyncResult <- deRefStablePtr envPtr

  mResult <- if (dataLen == 0)
    then pure Nothing
    else do
      bs <- BS8.packCStringLen (dataPtr, dataLen)
      case decode (BS.fromStrict bs) of
        Nothing -> error $ "jsaddle Results decode failed : "
        Just !r  -> pure $ Just r

  mBatch <- case (isSync, mResult) of
    (True, Nothing) -> error "processSyncResult need single result"
    (True, Just (r:[])) -> (:[]) <$> processSyncResult r
    (True, Just (r:_)) -> error "processSyncResult got multiple results"
    _ -> do
      traverse (traverse processResult) mResult
      -- Give the forked threads some time to work
      let
        loop m1 = do
          threadDelay (1000*1)
          m2 <- modifyMVar outgoingMessages $ \m -> pure ([], m)
          case m2 of
            [] -> pure m1
            _ -> loop (m1 ++ m2)
      loop []

  case mBatch of
    [] -> pure 0
    m -> do
      let outData = encode m
      -- putStrLn $ "outmsgsize: " <> show (BS.length outData)
      BSIO.useAsCStringLen (BS.toStrict outData) $ \(ptr, len) -> copyBytes dataPtr ptr len
      pure $ BS.length outData
