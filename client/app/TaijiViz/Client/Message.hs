{-# LANGUAGE OverloadedStrings #-}

module TaijiViz.Client.Message where

import qualified Data.ByteString as B
import Data.Serialize (encode)
import Reflex.Dom.Core
import Reflex.Dom.WebSocket
import Data.Serialize (decode)

import TaijiViz.Common.Types

import GHCJS.DOM.Types (liftJSM) 

sendMsg :: MonadWidget t m => Event t Command -> m (Event t Result)
sendMsg cmd = do
    ws <- webSocket url def
        { _webSocketConfig_send = fmap (return . encode) cmd
        }
    performEvent_ $ fmap (liftJSM . print) $ _webSocket_close ws
    return $ fromEither . decode <$> _webSocket_recv ws
  where
    url = "ws://yed.ucsd.edu:8787"
    fromEither (Left err) = error err
    fromEither (Right x) = x
