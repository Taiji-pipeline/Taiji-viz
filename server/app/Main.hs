{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent         (MVar, modifyMVar, modifyMVar_,
                                             newMVar, readMVar)
import           Control.Monad
import qualified Data.ByteString.Char8      as B
import qualified Data.Graph.Inductive.Graph as G
import           Data.Serialize             (decode, encode)
import qualified Network.WebSockets         as WS
import           System.Process             (runInteractiveProcess)
import System.IO

import           TaijiViz.Server.Workflow
import           TaijiViz.Common.Types      (Command (..), Result (..), ProgramStatus(..))
import           TaijiViz.Server.Types

import Control.Concurrent (threadDelay)

type ServerState = [Int]

sendGraph :: WS.Connection
          -> FilePath       -- ^ db file
          -> IO ()
sendGraph conn db = do
    svg <- getGraph
    graph <- layoutGraph' svg >>= drawGraph db
    WS.sendBinaryData conn $ encode $ Raw $ encode graph

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    (fromEither . decode) <$> WS.receiveData conn >>= handleMsg conn
  where
    fromEither (Left err) = error err
    fromEither (Right x)  = x

handleMsg :: WS.Connection -> Command -> IO ()
handleMsg conn msg = case msg of
    Connect _ -> sendGraph conn "sciflow.db"
    Run _     -> do
        threadDelay 2000000
        WS.sendBinaryData conn $ encode $ Status Running
        (i,o,e,p) <- runInteractiveProcess "taiji" ["run"] Nothing Nothing
        x <- hGetLine e
        putStrLn x
        threadDelay 2000000
        WS.sendBinaryData conn $ encode $ Status Stopped

main :: IO ()
main = do
    state <- newMVar []
    WS.runServer "0.0.0.0" 8787 $ application state
