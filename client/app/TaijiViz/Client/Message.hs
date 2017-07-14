{-# LANGUAGE OverloadedStrings #-}

module TaijiViz.Client.Message where

import qualified Data.ByteString as B
import Data.Serialize (encode)
import Reflex.Dom.Core
import Reflex.Dom.WebSocket

import TaijiViz.Common.Types

sendMsg :: MonadWidget t m => Event t Command -> m (Event t B.ByteString)
sendMsg cmd = do
    ws <- webSocket url def
        { _webSocketConfig_send = fmap (return . encode) cmd
        }
    return $ _webSocket_recv ws
  where
    url = "ws://yed.ucsd.edu:8787"
