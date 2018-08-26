{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Language.Javascript.JSaddle.Wasm

import Control.Lens ((^.))
import Language.Javascript.JSaddle
       (jsg, js, js1, jss, fun, valToNumber, syncPoint, eval)
import Language.Javascript.JSaddle.Object

main = do
  putStrLn "Hello from jsaddle-wasm-test"

  run $ do
    -- eval "globalFunc = function (x) {return x.length;}"
    -- v <- jsgf "globalFunc" ["World"]
    eval ("w = console; w.log('Hello World from inside of JSADDLE!')" :: [Char])
    doc <- jsg ("document" :: String)
    doc ^. js ("body" :: String) ^. jss ("innerHTML":: String) ("<h1>Kia ora (Hi)</h1>" :: String)
    -- eval ("console.log('from JSM!!!!');" :: [Char])
    return ()
    -- doc <- jsg ("document" :: Text)
    -- doc ^. js ("body" :: Text) ^. jss ("innerHTML" "<h1>Kia ora (Hi)</h1>" :: Text)

  putStrLn "Post JSM run, bye!"
  -- jsInOut <- openBinaryFile "/dev/jsaddle_inout" ReadWriteMode
  -- -- in <- openBinaryFile "/dev/jsaddle_in" ReadMode

  -- BS.hPut jsInOut "Hello jsaddle"
  -- BS.hPut jsInOut "More data to jsaddle"

  -- forkIO $ do
  --   BS.hPut jsInOut "Data from forked thread"
  --   threadDelay 1000
  --   BS.hPut jsInOut "Data from forked thread2"

  -- -- forkIO $ do
  -- --   putStrLn "Trying to read data"
  -- --   forever $ do
  -- --     threadDelay 1000000
  -- --     BS.hGetLine jsInOut
  -- --       >>= \v -> do
  -- --       putStr "GOT REPLY Fork!!: "
  -- --       putStr $ show v

  -- BS.hPut jsInOut "More data from Main thread"
  -- BS.hPut jsInOut "More data from Main thread2"

  -- let
  --   loop = do
  --     threadDelay 50000
  --     r <- try $ BS.hGetLine jsInOut
  --       >>= \v -> do
  --       putStr "GOT REPLY Main!!: "
  --       putStrLn $ show v
  --     case r of
  --       (Left (ex :: IOException)) -> return ()
  --       (Right _) -> loop
  -- loop

  -- putStrLn "Done"
