{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Javascript.JSaddle.Wasm (
  run
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString as BSIO

import Control.Monad (when, void, forever)
import Control.Concurrent (killThread, forkIO, threadDelay)
import Control.Exception (try, AsyncException, IOException, throwIO, fromException, finally)
import System.IO (openBinaryFile, IOMode(..))
import Data.Aeson (encode, decode)
import qualified Data.Binary as Binary

import Language.Javascript.JSaddle.Types (JSM, Batch, Results)
import Language.Javascript.JSaddle.Run (syncPoint, runJavaScript)
import Data.Word (Word32)

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
