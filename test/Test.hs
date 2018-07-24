{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import System.IO (openBinaryFile, IOMode(..))
import Control.Concurrent

main = do
  putStrLn "Hello from jsaddle-wasm-test"

  out <- openBinaryFile "/dev/jsaddle_out" WriteMode
  -- in <- openBinaryFile "/dev/jsaddle_in" ReadMode

  BS.hPut out "Hello jsaddle"
  BS.hPut out "More data to jsaddle"

  forkIO $ do
    BS.hPut out "Data from forked thread"
    threadDelay 1000
    BS.hPut out "Data from forked thread2"

  BS.hPut out "More data from Main thread"
  threadDelay 1000
  BS.hPut out "More data from Main thread2"

  putStrLn "Done"
