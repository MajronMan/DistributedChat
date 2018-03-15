module Lib
    ( someFunc
    ) where

import qualified Control.Concurrent             as CC
import qualified Control.Concurrent.STM.TBMChan as CCST
import qualified Control.Monad                  as CM
import qualified Control.Monad.Fix              as CMF
import qualified Control.Monad.IO.Class         as CMI
import qualified Control.Monad.STM              as CMS
import qualified Control.Monad.Trans.Resource   as CMTR
import qualified Data.ByteString.Char8          as B8
import qualified Data.Conduit                   as DC
import qualified Data.Conduit.Binary            as DCB
import qualified Data.Conduit.List              as DCL
import qualified Data.Conduit.TMChan            as DCT
import           Data.String
import qualified Network                        as N
import qualified Network.Multicast              as NM
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB
import qualified System.IO                      as SI


someFunc :: IO()
someFunc = client "localhost" 3000

localhost :: NS.HostAddress
localhost = NS.tupleToHostAddress (127,0,0,1)

multiHost = "224.0.0.1"
multiPort = 9999

createSocketUDP :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber ->  m (CMTR.ReleaseKey, NS.Socket)
createSocketUDP host port = do
  let hints = NS.defaultHints { NS.addrFlags = [NS.AI_NUMERICHOST], NS.addrSocketType = NS.Datagram }
  addr:_ <- CMI.liftIO $ NS.getAddrInfo (Just hints) (Just "127.0.0.1") Nothing
  CMTR.allocate
    (do
      sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
      NS.connect sock (NS.addrAddress addr)
      return sock
    )
    NS.close

createMultiSender :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber ->  m (CMTR.ReleaseKey, (NS.Socket, NS.SockAddr))
createMultiSender host port = CMTR.allocate (NM.multicastSender host port) (NS.close . fst)

createMultiReceiver :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber ->  m (CMTR.ReleaseKey, NS.Socket)
createMultiReceiver host port = CMTR.allocate (NM.multicastReceiver host port) NS.close

createSocketHandleTCP :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber -> m (CMTR.ReleaseKey, SI.Handle)
createSocketHandleTCP host port =  CMTR.allocate (N.connectTo host $ N.PortNumber $ fromIntegral port) SI.hClose

-- createReaderThread :: CMTR.MonadResource m => SI.Handle -> m (CMTR.ReleaseKey, CC.ThreadId)
-- createReaderThread socketHandle = CMTR.allocate (CC.forkIO $ CMTR.runResourceT $ streamInputToSocket socketHandle) CC.killThread

createReaderThread comm = CMTR.allocate (CC.forkIO $ CMTR.runResourceT $ streamInputToSockets comm) CC.killThread

-- streamInputToSocket :: CMI.MonadIO m => SI.Handle -> m ()
-- streamInputToSocket socketHandle = DCB.sourceHandle SI.stdin DC.$$ DCB.sinkHandle socketHandle

streamInputToSockets comm = DCB.sourceHandle SI.stdin DC.$$ conduitText comm

decodeMessage :: B8.ByteString -> (Int, B8.ByteString)
decodeMessage msg = case B8.readInt msg of
  Just (i, m) -> (i, B8.tail m)
  Nothing     -> (-1, msg)

displayChan :: CCST.TBMChan B8.ByteString -> Int -> IO () -> IO ()
displayChan chan cId loop = do
  maybeMsg <- CMS.atomically $ CCST.readTBMChan chan
  case maybeMsg of
    Nothing -> return () -- channel is closed
    Just msg -> do
      let (i, m) = decodeMessage msg
      CM.when (i /= cId) $ B8.putStr m
      loop

readFromSocket socket chan loop = do
  (msg, _) <- NSB.recvFrom socket 4096
  CMS.atomically $ CCST.writeTBMChan chan msg
  loop

streamSocketsToOutput :: SI.Handle -> NS.Socket -> Int -> CCST.TBMChan B8.ByteString -> IO ()
streamSocketsToOutput socketTCP socketMulti cId chan = do
  _ <- CC.forkIO $ CMF.fix $ readFromSocket socketMulti chan
  _ <- CC.forkIO $ CMF.fix $ displayChan chan cId
  DCB.sourceHandle socketTCP DC.$$ DCT.sinkTBMChan chan True

setLineBuffering :: SI.Handle -> IO()
setLineBuffering socketHandle = Prelude.mapM_ (`SI.hSetBuffering` SI.LineBuffering) [SI.stdin, SI.stdout, socketHandle]

testUDP host port = do
  (releaseSockUDP, socketUDP) <- createSocketUDP host port
  -- addr <- fmap Prelude.head (NS.getAddrInfo Nothing  (Just host) (Just (show port)))
  let servAddr = NS.SockAddrInet port localhost
  CMI.liftIO $ NSB.sendTo socketUDP "WITAM" servAddr
  -- CMI.liftIO $ SI.hPutStr socketHandleUDP "ELO"
  CMTR.release releaseSockUDP

-- client :: NS.HostName -> NS.PortNumber -> IO ()
-- client host port = CMTR.runResourceT $ testUDP host port

-- client :: NS.HostName -> NS.PortNumber -> IO ()
-- client host port = CMTR.runResourceT $ do
--   (releaseSockTCP, socketHandleTCP) <- createSocketHandleTCP host port
--   -- (releaseSockUDP, socketHandleUDP) <- createSocketHandleUDP host port

--   CMI.liftIO $ setLineBuffering socketHandleTCP -- inside ResourceT so need to lift
--   -- CMI.liftIO $ setLineBuffering socketHandleUDP

--   (releaseThread, _) <- createReaderThread socketHandleTCP
--   streamSocketToOutput socketHandleTCP

--   -- cleanup
--   CMTR.release releaseThread
--   CMTR.release releaseSockTCP
---------------------------------------------------------------------------
client :: NS.HostName -> NS.PortNumber -> IO ()
client host port = CMTR.runResourceT $ do
  (releaseSockTCP, socketHandleTCP) <- createSocketHandleTCP host port
  CMI.liftIO $ setLineBuffering socketHandleTCP -- inside ResourceT so need to lift
  rId <- CMI.liftIO (SI.hGetLine socketHandleTCP)
  let myId = read rId :: Int
  (releaseSockUDP, socketUDP) <- createSocketUDP host port
  (releaseMultiSender, (multiSender, addrMulti)) <- createMultiSender multiHost multiPort
  (releaseMultiReceiver, multiReceiver) <- createMultiReceiver multiHost multiPort

  chan <- CMI.liftIO $ CMS.atomically $ CCST.newTBMChan 16

  let comm = Communication {
    cId=myId,
    cHandleTCP=socketHandleTCP,
    cSocketUDP=socketUDP,
    cSocketMulti=multiSender,
    cAddrMulti=addrMulti
  }

  CMI.liftIO $ putStrLn "halko"
  (releaseThread, _) <- createReaderThread comm
  CMI.liftIO $ streamSocketsToOutput socketHandleTCP multiReceiver myId chan

  -- cleanup
  CMTR.release releaseThread
  CMTR.release releaseSockTCP
  CMTR.release releaseSockUDP
  CMTR.release releaseMultiSender
  CMTR.release releaseMultiReceiver

-- data Mode = Media | Text deriving Show
data Communication = Communication {
    cId          :: Int,
    cName        :: B8.ByteString,
    cHandleTCP   :: SI.Handle,
    cSocketUDP   :: NS.Socket,
    cSocketMulti :: NS.Socket,
    cAddrMulti   :: NS.SockAddr
  }

addIdToBuffer comm = B8.append (B8.append (B8.pack (show (cId comm))) ":")

conduitMedia ::(CMI.MonadIO m) => Communication -> B8.ByteString -> DC.ConduitM B8.ByteString o m ()
conduitMedia comm buffer = do
  -- Get all of the adjacent pairs from the stream
  mi1 <- DC.await

  case mi1 of
      Just i1->
          case i1 of
            "/U\r\n" -> do
              let msg = addIdToBuffer comm buffer
              CMI.liftIO $ NSB.sendTo (cSocketUDP comm) msg (NS.SockAddrInet 3000 localhost)
              conduitText comm
            _ -> conduitMedia comm (B8.append buffer i1)
      _ -> return ()

conduitText :: (CMI.MonadIO m) => Communication -> DC.ConduitM B8.ByteString o m ()
conduitText comm = do
  mi1 <- DC.await
  case mi1 of
      Just i1->
          case i1 of
            "U\r\n" -> conduitMedia comm ""
            "M\r\n" -> conduitMulti comm ""
            _ -> do
              CMI.liftIO $ SI.hPutStr (cHandleTCP comm) (B8.unpack i1)
              conduitText comm
      _ -> return ()

conduitMulti::(CMI.MonadIO m) => Communication -> B8.ByteString -> DC.ConduitM B8.ByteString o m ()
conduitMulti comm buffer = do
  mi1 <- DC.await
  case mi1 of
      Just i1->
          case i1 of
            "/M\r\n" -> do
              let msg = addIdToBuffer comm buffer
              CMI.liftIO $ NSB.sendTo (cSocketMulti comm) msg (cAddrMulti comm)
              conduitText comm
            _ -> conduitMulti comm (B8.append buffer i1)
      _ -> return ()

-- sink :: CMI.MonadIO m => DC.ConduitM (Mode, B8.ByteString) o m ()
-- sink = DC.awaitForever $ CMI.liftIO . putStrLn . \(a, b) -> show a ++ B8.unpack b
-- DCB.sourceHandle SI.stdin DC.$$ conduit DC.=$= sink


