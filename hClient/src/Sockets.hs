module Sockets where

import qualified Network.Socket                 as NS
import qualified Control.Monad.Fix              as CMF
import qualified System.IO                      as SI
import qualified Network.Socket.ByteString      as NSB
import qualified Control.Monad.STM              as CMS
import qualified Control.Concurrent.STM.TBMChan as CCST
import qualified Data.ByteString.Char8          as B8
import qualified Control.Exception            as CE
import qualified System.Exit as SE 

readFromSocket :: NS.Socket -> CCST.TBMChan B8.ByteString -> IO () -> IO ()
readFromSocket socket chan loop = do
  (msg, _) <- CE.handle 
    (\(CE.SomeException _) -> SE.exitFailure)
    (NSB.recvFrom socket 4096)
  CMS.atomically $ CCST.writeTBMChan chan msg
  loop

setLineBuffering :: SI.Handle -> IO()
setLineBuffering socketHandle = Prelude.mapM_ (`SI.hSetBuffering` SI.LineBuffering) [SI.stdin, SI.stdout, socketHandle]
