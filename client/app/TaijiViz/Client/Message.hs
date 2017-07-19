{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI            #-}

module TaijiViz.Client.Message where

import Data.Serialize (encode)
import Reflex.Dom.Core
import Data.Serialize (decode)
import qualified Data.Text as T

import TaijiViz.Common.Types

import GHCJS.DOM.Types (liftJSM, JSM)
import Data.JSString (JSString, unpack)

sendMsg :: MonadWidget t m => Event t Command -> m (Event t Result)
sendMsg cmd = do
    url <- T.pack . unpack <$> liftJSM js_getUrl
    ws <- webSocket url def
        { _webSocketConfig_send = fmap (return . encode) cmd
        }
    performEvent_ $ fmap (liftJSM . print) $ _webSocket_close ws
    return $ fromEither . decode <$> _webSocket_recv ws
  where
    fromEither (Left err) = error err
    fromEither (Right x) = x

foreign import javascript unsafe "'ws://'+window.location.hostname+':'+window.location.port"
    js_getUrl :: JSM JSString
