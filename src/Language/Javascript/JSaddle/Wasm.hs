{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Javascript.JSaddle.Wasm (
  run
  , run2
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString as BSIO

import Control.Monad (when, void, forever)
import Control.Concurrent (killThread, forkIO, threadDelay)
import Control.Exception (try, AsyncException, IOException, throwIO, fromException, finally)
import System.IO (openBinaryFile, IOMode(..))
import Data.Aeson (encode, decode)

import Language.Javascript.JSaddle.Types (JSM, Batch)
import Language.Javascript.JSaddle.Run (syncPoint, runJavaScript)

run2 :: JSM () -> IO ()
run2 entryPoint = do
  putStrLn "Starting JSaddle-Wasm"
  -- jsInOut <- openBinaryFile "/dev/jsaddle_inout" ReadWriteMode

  let
    sendBatch :: Batch -> IO ()
    sendBatch b = return ()

  runJavaScript sendBatch entryPoint

  return ()

run :: JSM () -> IO ()
run entryPoint = do
  putStrLn "Starting JSaddle-Wasm"

  jsInOut <- openBinaryFile "/dev/jsaddle_inout" ReadWriteMode

  let
    sendBatch :: Batch -> IO ()
    sendBatch b = BS.hPut jsInOut (encode b)

    receiveDataMessage :: IO (ByteString)
    receiveDataMessage = loop
      where
        loop = do
          threadDelay (1*1000*100)
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
    msg <- receiveDataMessage
    case decode msg of
      Nothing -> error $ "jsaddle Results decode failed : " <> show msg
      Just r  -> processResult r

  start
  waitTillClosed
