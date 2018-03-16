module TCP where

import qualified Network                        as N
import qualified Control.Monad.Trans.Resource   as CMTR
import qualified Network.Socket                 as NS
import qualified System.IO                      as SI

createSocketHandleTCP :: CMTR.MonadResource m => NS.HostName -> NS.PortNumber -> m (CMTR.ReleaseKey, SI.Handle)
createSocketHandleTCP host port =  CMTR.allocate (N.connectTo host $ N.PortNumber $ fromIntegral port) SI.hClose
