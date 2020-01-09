{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BangPatterns #-}

module Language.Javascript.JSaddle.Wasm (
  run
  , run2
  , jsaddleInit
  , jsaddleExecStep
  , HsEnv
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString as BSIO
import qualified Data.ByteString.Char8 as BS8

import Control.Monad (when, void, forever)
import Control.Concurrent (killThread, forkIO, threadDelay, yield)
import Control.Concurrent.MVar
import Control.Exception (try, AsyncException, IOException, throwIO, fromException, finally)
import System.IO (openBinaryFile, IOMode(..))
import Data.Aeson (encode, decode)
import qualified Data.Binary as Binary
import Data.IORef
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Marshal.Utils (copyBytes)
import Data.ByteString.Unsafe
import Data.Int (Int64)
import Language.Javascript.JSaddle.Types (JSM, Batch, Results)
import Language.Javascript.JSaddle.Run (syncPoint, runJavaScript)
import Data.Word (Word32)

run2 :: JSM () -> IO ()
run2 entryPoint = do
  putStrLn "Starting JSaddle-Wasm"
  -- jsInOut <- openBinaryFile "/dev/jsaddle_inout" ReadWriteMode

  let
    sendBatch :: Batch -> IO ()
    sendBatch b = return ()

  runJavaScript sendBatch entryPoint

  return ()

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
          threadDelay (100)
          try (BSIO.hGetLine jsInOut)
            >>= \case
              (Left (ex :: IOException)) -> loop
              (Right v) -> return $ BS.fromStrict v

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
processIncomingMsgs cont msgs = do
  let
    size = Binary.decode (BS.take 4 msgs) :: Word32
    (thisMsg, rest) = BS.splitAt (fromIntegral $ 4 + size) msgs
  case decode (BS.drop 4 thisMsg) of
    Nothing -> error $ "jsaddle Results decode failed : " <> show thisMsg
    Just r  -> cont r
  case BS.length rest of
    0 -> return ()
    _ -> processIncomingMsgs cont rest

data HsEnv = HsEnv
  { a :: MVar [Batch]
  , b :: MVar Results
  , c :: (Results -> IO ())
  }


jsaddleInit :: Int -> JSM () -> IO (StablePtr HsEnv)
jsaddleInit _ entryPoint = do
  putStrLn "Starting JSaddle-Wasm"
  outgoingMessages <- newMVar []
  incomingMessage <- newEmptyMVar
  lockInit <- newEmptyMVar

  let
    sendBatch :: Batch -> IO ()
    sendBatch b = do
      modifyMVar_ outgoingMessages $ \a -> pure (a ++ [b])
      tryPutMVar lockInit ()
      pure ()

  (processResult, _, start) <-
    runJavaScript sendBatch entryPoint
  putStrLn "runJavaScript done"

  forkIO $ start
  takeMVar lockInit
  putStrLn "start done"
  newStablePtr $ HsEnv outgoingMessages incomingMessage processResult

-- foreign export capi jsaddleExecStep :: StablePtr HsEnv -> CString -> Int -> IO Int64

-- jsaddleExecStep reads data from dataPtr, and writes back to same place
jsaddleExecStep :: StablePtr HsEnv -> CString -> Int -> IO Int64
jsaddleExecStep envPtr dataPtr dataLen = do
  putStrLn "Doing jsaddleExecStep"
  HsEnv outgoingMessages incomingMessage processResult <- deRefStablePtr envPtr
  when (dataLen /= 0) $ do
    bs <- BS8.packCStringLen (dataPtr, dataLen)
    case decode (BS.fromStrict bs) of
      Nothing -> error $ "jsaddle Results decode failed : "
      Just !r  -> processResult r
  let
    loop n = do
      yield
      message <- modifyMVar outgoingMessages $ \case
        [] -> pure ([], Nothing)
        (m:ms) -> pure (ms, Just m)
      case (message, n > 0) of
        (Nothing, True) -> loop (n - 1)
        (Nothing, False) -> pure 0
        (Just m, _) -> do
          let outData = encode m
          putStrLn $ "outmsgsize: " <> show (BS.length outData)
          BSIO.useAsCStringLen (BS.toStrict outData) $ \(ptr, len) -> copyBytes dataPtr ptr len
          pure $ BS.length outData
  loop 10
