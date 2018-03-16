module Messages where

import qualified Data.ByteString.Char8          as B8
import qualified Network.Socket                 as NS
import qualified System.IO                      as SI

type Msg = (Int, B8.ByteString)

data Communication = Communication {
    cId          :: Int,
    cHandleTCP   :: SI.Handle,
    cSocketUDP   :: NS.Socket,
    cSocketMulti :: NS.Socket,
    cAddrMulti   :: NS.SockAddr
  }

decodeMessage :: B8.ByteString -> Msg
decodeMessage msg = case B8.readInt msg of
  Just (i, m) -> (i, B8.tail m)
  Nothing     -> (-1, msg)

encodeMessage :: Communication -> B8.ByteString -> B8.ByteString
encodeMessage comm = B8.append (B8.pack (show (cId comm)) `B8.append` ":") 
