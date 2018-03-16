module Multicast where

import qualified Control.Monad.Trans.Resource   as CMTR
import qualified Network.Socket                 as NS
import qualified Network.Multicast              as NM

createMultiSender :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber 
  -> m (CMTR.ReleaseKey, (NS.Socket, NS.SockAddr))
createMultiSender host port = CMTR.allocate (NM.multicastSender host port) (NS.close . fst)

createMultiReceiver :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber 
  ->  m (CMTR.ReleaseKey, NS.Socket)
createMultiReceiver host port = CMTR.allocate (NM.multicastReceiver host port) NS.close
