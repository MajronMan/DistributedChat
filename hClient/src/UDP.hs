module UDP where 

import qualified Control.Monad.Trans.Resource   as CMTR
import qualified Network.Socket                 as NS
import qualified System.IO                      as SI
import qualified Control.Monad.IO.Class         as CMI

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
