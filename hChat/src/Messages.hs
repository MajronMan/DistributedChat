module Messages where

import qualified Data.ByteString.Char8        as B8

type Msg = (Int, B8.ByteString)

decodeMessage :: B8.ByteString -> (Int, B8.ByteString)
decodeMessage msg = case B8.readInt msg of
  Just (i, m) -> (i, B8.tail m)
  Nothing     -> (-1, msg)
