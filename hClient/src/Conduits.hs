module Conduits where

import qualified Control.Monad.IO.Class         as CMI
import qualified Data.ByteString.Char8          as B8
import qualified Data.Conduit                   as DC
import qualified Network.Socket.ByteString      as NSB
import qualified System.IO                      as SI
import qualified Network.Socket                 as NS
import qualified Control.Concurrent.STM.TBMChan as CCST
import qualified Control.Concurrent             as CC
import qualified Data.Conduit.Binary            as DCB
import qualified Control.Monad.Fix              as CMF
import qualified Data.Conduit.TMChan            as DCT
import qualified Control.Exception            as CE
import qualified Control.Monad.Trans.Resource   as CMTR
import qualified System.Exit as SE 

import Messages
import ServerData
import Sockets 
import Channels

conduitMedia ::(CMI.MonadIO m) => Communication -> B8.ByteString -> DC.ConduitM B8.ByteString o m ()
conduitMedia comm buffer = do
  mi1 <- DC.await

  case mi1 of
      Just i1->
          case i1 of
            "/U\r\n" -> do
              let msg = encodeMessage comm buffer
              CMI.liftIO $ NSB.sendTo (cSocketUDP comm) msg localServer
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
              CMI.liftIO $ 
                CE.handle 
                  (\(CE.SomeException _) -> SE.die "Server closed the connection") 
                  (SI.hPutStr (cHandleTCP comm) (B8.unpack i1))
              conduitText comm
      _ -> return ()

conduitMulti::(CMI.MonadIO m) => Communication -> B8.ByteString -> DC.ConduitM B8.ByteString o m ()
conduitMulti comm buffer = do
  mi1 <- DC.await
  case mi1 of
      Just i1->
          case i1 of
            "/M\r\n" -> do
              let msg = encodeMessage comm buffer
              CMI.liftIO $ NSB.sendTo (cSocketMulti comm) msg (cAddrMulti comm)
              conduitText comm
            _ -> conduitMulti comm (B8.append buffer i1)
      _ -> return ()

streamInputToSockets :: CMI.MonadIO m => Communication -> m ()
streamInputToSockets comm = DCB.sourceHandle SI.stdin DC.$$ conduitText comm

breakLoop :: CE.SomeException -> IO ()
breakLoop (CE.SomeException _) = return ()

sourceTCP socketTCP = DC.bracketP 
              (return socketTCP)
              SI.hClose
              (\s -> DCB.sourceHandle s)

streamSocketsToOutput :: SI.Handle -> NS.Socket -> Int -> CCST.TBMChan B8.ByteString -> IO ()
streamSocketsToOutput socketTCP socketMulti cId chan = do
  _ <- CC.forkIO $ CMF.fix $ readFromSocket socketMulti chan
  _ <- CC.forkIO $ CMF.fix $ displayChan chan cId
  CMTR.runResourceT $ (sourceTCP socketTCP) DC.$$ DCT.sinkTBMChan chan True
