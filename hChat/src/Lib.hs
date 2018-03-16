module Lib
    ( runServer
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

import           Sockets
import           TCP
import           UDP
import           Channels

runServer :: IO ()
runServer = do
  sockTCP <- createAndBindSocket 3000 NS.Stream
  sockUDP <- createAndBindSocket 3000 NS.Datagram
  NS.listen sockTCP 5
  B8.putStrLn "Listening on port 3000"

  chan <- CMS.atomically TC.newTChan
  clientCount <- CMS.atomically TC.newTChan
  CMS.atomically $ (TC.writeTChan clientCount 0)
  _ <- CC.forkIO $ CMF.fix $ serverChannelLoop chan
  _ <- CC.forkIO $ CMF.fix $ serverLoopUDP sockUDP chan

  acceptClientLoop sockTCP chan clientCount 0
