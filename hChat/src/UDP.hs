module UDP where

import qualified Network.Socket               as NS
import qualified Network.Socket.ByteString    as NSB
import qualified Control.Concurrent.STM.TChan as TC

import Channels (writeToChannel)
import Messages

serverLoopUDP :: NS.Socket -> TC.TChan Msg -> IO a -> IO a
serverLoopUDP sock chan loop = do
  (coded, _) <- NSB.recvFrom sock 4096
  let (clId, message) = decodeMessage coded

  writeToChannel chan message clId
  loop
