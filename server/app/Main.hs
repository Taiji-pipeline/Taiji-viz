{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Codec.Compression.GZip         (decompress)
import           Control.Concurrent             (MVar, newMVar, readMVar, modifyMVar_)
import           Control.Exception              (bracket)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Binary                    (decode)
import Data.Maybe (fromJust)
import Shelly hiding (FilePath)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setHost, setPort)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (defaultConnectionOptions)
import           Scientific.Workflow.DB         (closeDB, openDB, readData)
import           Servant

import           TaijiViz.Common.Types
import           TaijiViz.Server.Http           (httpApp)
import           TaijiViz.Server.Socket         (ServerState (..),
                                                 defaultServerState, socketApp)

main :: IO ()
main = do
    state <- newMVar defaultServerState
    runSettings (setHost "127.0.0.1" $ setPort 8787 $ defaultSettings) $
        serve (Proxy :: Proxy API) $ server state

type API = "data" :> "table" :> Get '[JSON] (Maybe RankTable)
      :<|> Raw

server :: MVar ServerState -> Server API
server state = sendRankTable
       :<|> websocketsOr defaultConnectionOptions (socketApp state) httpApp
  where
    sendRankTable = liftIO $ do
        r <- _final_result <$> readMVar state
        case r of
            Just x -> return $ Just $ ranktable x
            Nothing -> do
                st <- _node_status <$> readMVar state
                case M.lookup "Export_results" st of
                    Just Finished -> do
                        wd <- fromJust . _current_wd <$> readMVar state
                        let dbfile = T.unpack wd ++ "/sciflow.db"
                        bracket (openDB dbfile) closeDB $ \db -> do
                            file <- readData "Export_results" db
                            results <- fmap (decode . decompress . BL.fromStrict) $ shelly $
                                chdir (fromText wd) $ readBinary $ fromText $ T.pack file
                            modifyMVar_ state $ \x -> return x{_final_result=Just results}
                            return $ Just $ ranktable results
                    _ -> return Nothing
