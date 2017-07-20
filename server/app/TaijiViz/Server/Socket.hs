{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Server.Socket
    ( ServerState(..)
    , defaultServerState
    , socketApp
    ) where

import           Conduit
import           Control.Concurrent       (MVar, modifyMVar_, readMVar)
import           Control.Exception        (bracket)
import qualified Data.ByteString.Char8    as B
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import           Data.Serialize           (decode, encode)
import qualified Data.Text                as T
import qualified Network.WebSockets       as WS
import           Scientific.Workflow.DB   (closeDB, isFinished, openDB)
import           Shelly                   (shelly, test_f, fromText)
import           System.Process           (CreateProcess (..), ProcessHandle,
                                           StdStream (..), createProcess,
                                           interruptProcessGroupOf, proc)

import           TaijiViz.Common.Types    (Command (..), NodeState (..),
                                           ProgramStatus (..), Result (..))
import           TaijiViz.Server.Workflow

import           Debug.Trace

data ServerState = ServerState
    { _current_process :: Maybe ProcessHandle
    , _current_wd      :: Maybe T.Text
    , _is_running      :: Bool
    , _node_status     :: M.Map T.Text NodeState
    }

defaultServerState :: ServerState
defaultServerState = ServerState
    { _current_process = Nothing
    , _current_wd      = Nothing
    , _is_running      = False
    , _node_status = M.empty
    }

sendResult :: WS.Connection -> Result -> IO ()
sendResult conn r = do
    traceM $ "Sending: " ++ show r
    WS.sendBinaryData conn $ encode r

sendGraph :: WS.Connection
          -> FilePath       -- ^ db file
          -> IO ()
sendGraph conn db = do
    graph <- getGraph >>= layoutGraph'
    exist <- shelly $ test_f $ fromText $ T.pack db
    graph' <- if exist
        then bracket (openDB db) closeDB $ \h -> drawGraph (flip isFinished h) graph
        else drawGraph (const (return False)) graph
    WS.sendBinaryData conn $ encode $ Raw $ encode graph'

socketApp :: MVar ServerState -> WS.ServerApp
socketApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    (fromEither . decode) <$> WS.receiveData conn >>= handleMsg state conn
  where
    fromEither (Left err) = error err
    fromEither (Right x)  = x

handleMsg :: MVar ServerState -> WS.Connection -> Command -> IO ()
handleMsg state conn msg = case msg of
    Connect -> do
        st <- readMVar state
        case _current_wd st of
            Nothing -> return ()
            Just wd -> do
                sendGraph conn $ T.unpack wd ++ "/sciflow.db"
                if _is_running st
                    then sendResult conn $ Status Running
                    else sendResult conn $ Status Stopped
    SetCWD wd -> do
        modifyMVar_ state $ \x -> return x{_current_wd = Just wd}
        sendGraph conn $ T.unpack wd ++ "/sciflow.db"
    Run selected     -> do
        st <- readMVar state
        if _is_running st
            then stopTaiji state (fromJust $ _current_process st) conn
            else runTaiji state selected conn
    Delete selected -> undefined

runTaiji :: MVar ServerState -> [T.Text] -> WS.Connection -> IO ()
runTaiji state selected conn = bracket
    ( do wd <- _current_wd <$> readMVar state
         (_, _, Just e, p) <- createProcess cmd
            { cwd = T.unpack <$> wd
            , std_err = CreatePipe
            , new_session = True }
         modifyMVar_ state $ \x -> return x
            {_current_process = Just p, _is_running = True}
         sendResult conn $ Status Running
         return e )
    ( \_ -> do modifyMVar_ state $ \x -> return x{_is_running=False}
               sendResult conn $ Status Stopped )
    ( \e -> sourceHandle e =$= linesUnboundedAsciiC =$= concatMapMC fn $$
                mapM_C (sendResult conn) )
  where
    cmd = proc "taiji" $ ["run", "--config", "config.yml"] ++ selection
    selection | null selected = []
              | otherwise = ["--select", T.unpack $ T.intercalate "," selected]
    fn x = do
        B.putStrLn x
        return $ processMsg $ strip x

stopTaiji :: MVar ServerState -> ProcessHandle -> WS.Connection -> IO ()
stopTaiji state p conn = do
    interruptProcessGroupOf p
    modifyMVar_ state $ \x -> return x{_is_running=False}
    sendResult conn $ Status Stopped


processMsg :: B.ByteString -> Maybe Result
processMsg msg | isMsg msg = Just result
               | isErr msg = Just $ Exception $ T.pack $ B.unpack msg
               | otherwise = Nothing
  where
    isMsg x = "[LOG][" `B.isPrefixOf` x || "[WARN][" `B.isPrefixOf` x
    isErr = B.isPrefixOf "[ERROR]["
    result = let [state, pid] = take 2 $ reverse $ B.words msg
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
