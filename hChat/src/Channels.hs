module Channels where

import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Monad.STM            as CMS
import qualified System.IO                    as SI
import qualified Data.ByteString.Char8        as B8
import qualified Control.Monad                as CM
import qualified Control.Monad.IO.Class as CMI

import Messages

writeToChannel :: TC.TChan Msg -> B8.ByteString -> Int -> IO ()
writeToChannel chan msg msgNum = CMS.atomically $ TC.writeTChan chan (msgNum, msg)

changeValue chan by = CMS.atomically $ do 
  value <- TC.readTChan chan 
  let newVal = value + by
  TC.writeTChan chan newVal 
  return newVal

serverChannelLoop :: TC.TChan Msg -> IO a -> IO a
serverChannelLoop chan loop = do
  (cId, msg) <- CMS.atomically $ TC.readTChan chan
  B8.putStrLn $ B8.pack (show cId) `B8.append` ":" `B8.append` msg
  loop

clientReadChannelLoop :: SI.Handle -> TC.TChan Msg -> Int -> IO a -> IO a
clientReadChannelLoop hdl chan msgNum loop = do
  (nextNum, line) <- CMS.atomically $ TC.readTChan chan
  CM.when (msgNum /= nextNum) $ B8.hPutStrLn hdl line -- do not print at sender
  loop

clientWriteChannelLoop :: SI.Handle -> TC.TChan Msg -> B8.ByteString -> Int -> IO () -> IO ()
clientWriteChannelLoop hdl chan name msgNum loop = do
  line <- fmap B8.init (B8.hGetLine hdl)
  case line of
        "quit" -> B8.hPutStrLn hdl "Bye!"
        _      -> do 
          let msg = name `B8.append` ": " `B8.append` line
          writeToChannel chan msg msgNum 
          loop
