module Lib
    ( runClient
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
import qualified System.Exit as SE

import Sockets
import UDP
import TCP 
import Multicast 
import Conduits
import ServerData
import Messages

runClient :: IO()
runClient = client "localhost" 3000

createReaderThread :: CMTR.MonadResource m => Communication 
  -> m (CMTR.ReleaseKey, CC.ThreadId)
createReaderThread comm = CMTR.allocate 
  (CC.forkIO $ CMTR.runResourceT $ streamInputToSockets comm) 
  CC.killThread

client :: NS.HostName -> NS.PortNumber -> IO ()
client host port = CMTR.runResourceT $ do
  (releaseSockTCP, socketHandleTCP) <- createSocketHandleTCP host port
  CMI.liftIO $ setLineBuffering socketHandleTCP -- inside ResourceT so need to lift
  rId <- CMI.liftIO (SI.hGetLine socketHandleTCP)
  let myId = read rId :: Int
  if myId == -1 
    then CMI.liftIO $ SE.die "Server has too many clients"
  else do
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

    (releaseThread, _) <- createReaderThread comm
    CMI.liftIO $ streamSocketsToOutput socketHandleTCP multiReceiver myId chan

    -- cleanup
    CMTR.release releaseThread
    CMTR.release releaseSockTCP
    CMTR.release releaseSockUDP
    CMTR.release releaseMultiSender
    CMTR.release releaseMultiReceiver
