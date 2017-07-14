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

import           Control
import           TaijiViz.Common.Types      (Command (..))
import           Types

type ServerState = [Int]

sendGraph :: WS.Connection -> IO ()
sendGraph conn = do
    svg <- getGraph
    layout <- layoutGraph' svg
    WS.sendBinaryData conn $ encode $ drawGraph layout

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ (fromEither . decode) <$> WS.receiveData conn >>= handleMsg conn
  where
    fromEither (Left err) = error err
    fromEither (Right x)  = x

handleMsg :: WS.Connection -> Command -> IO ()
handleMsg conn msg = case msg of
    Connect _ -> sendGraph conn
    Run _     -> do
        (i,o,e,p) <- runInteractiveProcess "taiji" ["run"] Nothing Nothing
        x <- hGetLine e
        putStrLn x
main :: IO ()
main = do
    state <- newMVar []
    svg <- getGraph
    layout <- layoutGraph' svg
    print $ map (snd . snd) $ fst $ drawGraph layout
    WS.runServer "0.0.0.0" 8787 $ application state
