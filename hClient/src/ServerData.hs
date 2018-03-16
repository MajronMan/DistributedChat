module ServerData where

import qualified Network.Socket                 as NS

localhost :: NS.HostAddress
localhost = NS.tupleToHostAddress (127,0,0,1)

localPort :: NS.PortNumber 
localPort = 3000

localServer :: NS.SockAddr
localServer = NS.SockAddrInet localPort localhost

multiHost = "224.0.0.1"

multiPort :: NS.PortNumber 
multiPort = 9999
