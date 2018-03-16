module TCP where

import qualified Control.Concurrent           as CC
import qualified Network.Socket               as NS
import qualified Data.ByteString.Char8        as B8
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Exception            as CE
import qualified System.IO                    as SI
import qualified Control.Monad.Fix            as CMF
import qualified Control.Monad.STM            as CMS

import Messages
import Sockets (connToHandle)
import Channels

acceptClientLoop :: NS.Socket -> TC.TChan Msg -> Int -> IO ()
acceptClientLoop sock chan msgNum = do
  putStrLn "Awaiting for connections"
  hdl <- NS.accept sock >>= connToHandle
  putStrLn "Accepted new connection"
  CC.forkIO (runConn hdl chan msgNum)
  acceptClientLoop sock chan $! msgNum + 1

breakLoop :: CE.SomeException -> IO ()
breakLoop (CE.SomeException _) = return ()

welcome :: SI.Handle -> TC.TChan Msg -> Int -> IO B8.ByteString
welcome hdl chan msgNum = do
  B8.hPutStrLn hdl (B8.pack $ show msgNum)
  B8.hPutStrLn hdl "What's your name?"

  name <- fmap B8.init (B8.hGetLine hdl) -- discard \n

  writeToChannel chan ("--> " `B8.append` name `B8.append` " entered chat.") msgNum
  B8.hPutStrLn hdl ("Welcome, " `B8.append` name `B8.append` "!")

  return name

runConn :: SI.Handle -> TC.TChan Msg -> Int -> IO ()
runConn hdl chan msgNum = do
    name <- welcome hdl chan msgNum
    commLine <- CMS.atomically $ TC.dupTChan chan
    reader <- CC.forkIO $ CMF.fix $ clientReadChannelLoop hdl commLine msgNum
    CE.handle breakLoop $ CMF.fix $ clientWriteChannelLoop hdl chan name msgNum

    CC.killThread reader
    writeToChannel chan ("<-- " `B8.append` name `B8.append` " left.") msgNum
    SI.hClose hdl
