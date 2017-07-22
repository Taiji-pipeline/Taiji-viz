{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI            #-}

module TaijiViz.Client.Message
    ( sendMsg
    , httpUrl
    , socketUrl
    ) where

import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BL
import Reflex.Dom.Core
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import GHCJS.DOM.Types (liftJSM, JSM)
import Data.JSString (JSString, unpack)

import TaijiViz.Common.Types

baseUrl :: T.Text
baseUrl = unsafePerformIO $ T.pack . unpack <$> liftJSM js_getUrl

socketUrl :: T.Text
socketUrl = "ws://" `T.append` baseUrl
{-# INLINE socketUrl #-}

httpUrl :: T.Text
httpUrl = "http://" `T.append` baseUrl
{-# INLINE httpUrl #-}

sendMsg :: MonadWidget t m => Event t Command -> m (Event t Result)
sendMsg cmd = do
    ws <- webSocket socketUrl def
        { _webSocketConfig_send = fmap (return . BL.toStrict . encode) cmd
        , _webSocketConfig_reconnect = False
        }
    performEvent_ $ fmap (liftJSM . print) $ _webSocket_close ws
    return $ decode . BL.fromStrict <$> _webSocket_recv ws
  where
    fromEither (Left err) = error err
    fromEither (Right x) = x

foreign import javascript unsafe "window.location.hostname+':'+window.location.port"
    js_getUrl :: JSM JSString
