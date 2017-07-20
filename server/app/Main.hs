{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent             (newMVar)
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setHost, setPort)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS

import           TaijiViz.Server.Http           (httpApp)
import           TaijiViz.Server.Socket         (defaultServerState, socketApp)

main :: IO ()
main = do
    state <- newMVar defaultServerState
    runSettings (setHost "127.0.0.1" $ setPort 8787 $ defaultSettings) $
        websocketsOr WS.defaultConnectionOptions (socketApp state) httpApp
