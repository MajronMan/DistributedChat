module Sockets where

import qualified Network.Socket as NS
import qualified System.IO                    as SI

createAndBindSocket :: NS.PortNumber -> NS.SocketType -> IO NS.Socket
createAndBindSocket port sockType = do
  sock <- NS.socket NS.AF_INET sockType 0
  NS.setSocketOption sock NS.ReuseAddr 1
  let addr = NS.SockAddrInet port NS.iNADDR_ANY
  NS.bind sock addr
  return sock

connToHandle (sock, addr) = do
  print addr
  hdl <- NS.socketToHandle sock SI.ReadWriteMode
  SI.hSetBuffering hdl SI.LineBuffering
  return hdl
