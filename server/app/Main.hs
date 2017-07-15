{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Conduit
import           Control.Concurrent         (MVar, newMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8      as B
import qualified Data.Graph.Inductive.Graph as G
import           Data.Serialize             (decode, encode)
import qualified Data.Text                  as T
import qualified Network.WebSockets         as WS
import           System.IO
import           System.Process             (ProcessHandle, getProcessExitCode,
                                             runInteractiveProcess,
                                             terminateProcess)

import           TaijiViz.Common.Types      (Command (..), NodeState (..),
                                             ProgramStatus (..), Result (..))
import           TaijiViz.Server.Types
import           TaijiViz.Server.Workflow

import           Control.Concurrent         (threadDelay)
import           Debug.Trace

type ServerState = Maybe ProcessHandle

sendResult :: WS.Connection -> Result -> IO ()
sendResult conn r = do
    traceM $ "Sending: " ++ show r
    WS.sendBinaryData conn $ encode r

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
    (fromEither . decode) <$> WS.receiveData conn >>= handleMsg state conn
  where
    fromEither (Left err) = error err
    fromEither (Right x)  = x

handleMsg :: MVar ServerState -> WS.Connection -> Command -> IO ()
handleMsg state conn msg = case msg of
    Connect _ -> sendGraph conn "sciflow.db"
    Run _     -> do
        process <- takeMVar state
        case process of
            Nothing -> startProgram
            Just h -> do
                exitCode <- getProcessExitCode h
                case exitCode of
                    Nothing -> do
                        terminateProcess h
                        traceM "process stopped"
                        putMVar state Nothing
                    Just _ -> startProgram
  where
    startProgram = do
        threadDelay 2000000
        sendResult conn $ Status Running
        (_,_,e,p) <- runInteractiveProcess "taiji" ["run", "--config", "config.yml"] Nothing Nothing
        traceM "process started"
        putMVar state $ Just p
        sourceHandle e =$= linesUnboundedAsciiC =$=
            concatMapC (processMsg . strip) $$
            mapM_C (sendResult conn)
        threadDelay 2000000
        sendResult conn $ Status Stopped

processMsg :: B.ByteString -> Maybe Result
processMsg x | isMsg x = Just result
             | otherwise = Nothing
  where
    isMsg x = "[LOG][" `B.isPrefixOf` x || "[WARN][" `B.isPrefixOf` x
    result = let [state, pid] = take 2 $ reverse $ B.words x
                 st = case () of
                     _ | "running" `B.isPrefixOf` state -> InProgress
                       | "Failed" `B.isPrefixOf` state -> Failed
                       | "Finished" `B.isPrefixOf` state -> Finished
                       | otherwise -> Unknown
              in Notification (T.init $ T.pack $ B.unpack pid) st

strip :: B.ByteString -> B.ByteString
strip = B.pack . reverse . fst . B.foldl' f ([], False)
  where
    f (acc, isESC) x = if isESC
        then if x == 'm' then (acc, False) else (acc, isESC)
        else if x == '\ESC' then (acc, True) else (x:acc, isESC)

main :: IO ()
main = do
    state <- newMVar Nothing
    WS.runServer "0.0.0.0" 8787 $ application state
