module Lib
    ( someFunc
    ) where

import qualified Control.Concurrent           as CC
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Exception            as CE
import qualified Control.Monad                as CM
import qualified Control.Monad.Fix            as CMF
import qualified Control.Monad.STM            as CMS
import qualified Data.ByteString.Char8        as B8
import qualified Network.Multicast            as NM
import qualified Network.Socket               as NS
import qualified Network.Socket.ByteString    as NSB
import qualified System.IO                    as SI

createAndBindSocket :: NS.PortNumber -> NS.SocketType -> IO NS.Socket
createAndBindSocket port sockType = do
  sock <- NS.socket NS.AF_INET sockType 0
  NS.setSocketOption sock NS.ReuseAddr 1
  let addr = NS.SockAddrInet port NS.iNADDR_ANY
  NS.bind sock addr
  return sock

serverChannelLoop :: TC.TChan Msg -> IO a -> IO a
serverChannelLoop chan loop = do
  (cId, msg) <- CMS.atomically $ TC.readTChan chan
  putStrLn $  show cId ++ ":" ++ msg
  loop

decodeMessage msg = case B8.readInt msg of
  Just (i, m) -> (i, B8.tail m)
  Nothing     -> (-1, msg)

serverLoopUDP :: NS.Socket -> TC.TChan Msg -> IO a -> IO a
serverLoopUDP sock chan loop = do
  (coded, _) <- NSB.recvFrom sock 4096
  let (clId, message) = decodeMessage coded

  broadcast chan (B8.unpack message) clId
  loop

clientChannelReceiveLoop :: SI.Handle -> TC.TChan Msg -> Int -> IO a -> IO a
clientChannelReceiveLoop hdl chan msgNum loop = do
  (nextNum, line) <- CMS.atomically $ TC.readTChan chan
  CM.when (msgNum /= nextNum) $ SI.hPutStrLn hdl line -- do not print at sender
  loop

clientChannelSendLoop hdl chan name msgNum loop = do
  line <- fmap init (SI.hGetLine hdl)
  case line of
        "quit" -> SI.hPutStrLn hdl "Bye!"
        _      -> broadcast chan (name ++ ": " ++ line) msgNum >> loop

someFunc :: IO ()
someFunc = do
  sockTCP <- createAndBindSocket 3000 NS.Stream
  NS.listen sockTCP 5
  sockUDP <- createAndBindSocket 3000 NS.Datagram
  putStrLn "Listening on port 3000"
  chan <- CMS.atomically TC.newTChan
  _ <- CC.forkIO $ CMF.fix $ serverChannelLoop chan
  _ <- CC.forkIO $ CMF.fix $ serverLoopUDP sockUDP chan
  mainLoop sockTCP chan 0

-- someFunc = testUDP

type Msg = (Int, String)

connToHandle (sock, addr) = do
  print addr
  hdl <- NS.socketToHandle sock SI.ReadWriteMode
  SI.hSetBuffering hdl SI.LineBuffering
  return hdl

mainLoop :: NS.Socket -> TC.TChan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  putStrLn "Awaiting for connections"
  hdl <- NS.accept sock >>= connToHandle
  putStrLn "Accepted new connection"
  CC.forkIO (runConn hdl chan msgNum)
  mainLoop sock chan $! msgNum + 1

broadcast :: TC.TChan Msg -> String -> Int -> IO ()
broadcast chan msg msgNum = CMS.atomically $ TC.writeTChan chan (msgNum, msg)

welcome :: SI.Handle -> TC.TChan Msg -> Int -> IO String
welcome hdl chan msgNum = do
  SI.hPrint hdl msgNum
  SI.hPutStrLn hdl "What's your name?"
  name <- fmap init (SI.hGetLine hdl)
  broadcast chan ("--> " ++ name ++ " entered chat.") msgNum
  SI.hPutStrLn hdl ("Welcome, " ++ name ++ "!")

  return name

breakLoop (CE.SomeException _) = return ()

runConn :: SI.Handle -> TC.TChan Msg -> Int -> IO ()
runConn hdl chan msgNum = do
    name <- welcome hdl chan msgNum
    commLine <- CMS.atomically $ TC.dupTChan chan
    reader <- CC.forkIO $ CMF.fix $ clientChannelReceiveLoop hdl commLine msgNum
    CE.handle breakLoop $ CMF.fix $ clientChannelSendLoop hdl chan name msgNum

    CC.killThread reader
    broadcast chan ("<-- " ++ name ++ " left.") msgNum
    SI.hClose hdl


testLoop socket loop = do
  (msg, addr) <- NSB.recvFrom socket 4096
  putStrLn (B8.unpack msg)
  loop

testUDP = do
  sockUDP <- createAndBindSocket 3000 NS.Datagram
  CMF.fix $ testLoop sockUDP
