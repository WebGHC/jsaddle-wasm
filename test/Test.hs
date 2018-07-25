{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import System.IO (openBinaryFile, IOMode(..))
import Control.Concurrent
import Control.Monad
import Control.Exception

main = do
  putStrLn "Hello from jsaddle-wasm-test"

  jsInOut <- openBinaryFile "/dev/jsaddle_inout" ReadWriteMode
  -- in <- openBinaryFile "/dev/jsaddle_in" ReadMode

  BS.hPut jsInOut "Hello jsaddle"
  BS.hPut jsInOut "More data to jsaddle"

  forkIO $ do
    BS.hPut jsInOut "Data from forked thread"
    threadDelay 1000
    BS.hPut jsInOut "Data from forked thread2"

  -- forkIO $ do
  --   putStrLn "Trying to read data"
  --   forever $ do
  --     threadDelay 1000000
  --     BS.hGetLine jsInOut
  --       >>= \v -> do
  --       putStr "GOT REPLY Fork!!: "
  --       putStr $ show v

  BS.hPut jsInOut "More data from Main thread"
  BS.hPut jsInOut "More data from Main thread2"

  let
    loop = do
      threadDelay 50000
      r <- try $ BS.hGetLine jsInOut
        >>= \v -> do
        putStr "GOT REPLY Main!!: "
        putStrLn $ show v
      case r of
        (Left (ex :: IOException)) -> return ()
        (Right _) -> loop
  loop

  putStrLn "Done"
