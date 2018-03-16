module Channels where 

import qualified Control.Concurrent.STM.TBMChan as CCST
import qualified Control.Monad.STM              as CMS
import qualified Data.ByteString.Char8          as B8
import qualified Control.Monad                  as CM
import qualified Control.Exception            as CE
import qualified System.Exit as SE 

import Messages

displayChan :: CCST.TBMChan B8.ByteString -> Int -> IO () -> IO ()
displayChan chan cId loop = do
  maybeMsg <- CE.handle 
    (\(CE.SomeException _) -> SE.exitFailure) 
    (CMS.atomically $ CCST.readTBMChan chan)
  case maybeMsg of
    Nothing -> return () -- channel is closed
    Just msg -> do
      let (i, m) = decodeMessage msg
      CM.when (i /= cId) $ B8.putStr m
      loop
