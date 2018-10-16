{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Codec.Compression.GZip          (decompress)
import           Control.Concurrent              (MVar, forkIO, modifyMVar_,
                                                  newMVar, readMVar)
import           Control.Exception               (bracket)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Binary                     (decode)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromJust)
import qualified Data.Text                       as T
import           Network.Wai.Handler.Warp        (defaultSettings, runSettings,
                                                  setHost, setPort)
import           Network.Wai.Handler.WebSockets  (websocketsOr)
import           Network.WebSockets              (defaultConnectionOptions)
import           Scientific.Workflow.Internal.DB (closeDB, openDB, readData, deserialize)
import           Servant
import           Shelly                          hiding (FilePath)

import           TaijiViz.Common.Types
import           TaijiViz.Server.Http            (httpApp)
import           TaijiViz.Server.Socket          (ServerState (..),
                                                  defaultServerState, readLog,
                                                  socketApp)

main :: IO ()
main = do
    state <- newMVar defaultServerState
    forkIO $ readLog state
    putStrLn "Server is running at: 127.0.0.1:8787"
    runSettings (setHost "127.0.0.1" $ setPort 8787 $ defaultSettings) $
        serve (Proxy :: Proxy API) $ server state

type API = Raw

server :: MVar ServerState -> Server API
server state = Tagged (websocketsOr defaultConnectionOptions (socketApp state) httpApp)