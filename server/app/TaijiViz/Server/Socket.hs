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
import qualified Data.Map.Strict                 as M
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
    , _main_conn :: Maybe WS.Connection
    }

defaultServerState :: ServerState
defaultServerState = ServerState
    { _current_process = Nothing
    , _current_wd      = Nothing
    , _is_running      = False
    , _node_status = M.empty
    , _main_conn = Nothing
    }

socketApp :: MVar ServerState -> WS.ServerApp
socketApp state pending = do
    currentConn <- _main_conn <$> readMVar state
    case currentConn of
        Just _ -> return ()
        Nothing -> do
            conn <- WS.acceptRequest pending
            WS.forkPingThread conn 30
            msg <- (fromEither . decode) <$> WS.receiveData conn
            case msg of
                Connect -> flip finally (disconnect state) $ do
                    modifyMVar_ state $ \x -> x{_main_conn=Just conn}
                    print "connected"
                    initialize state conn
                    forever $ fromEither . decode <$> WS.receiveData conn >>=
                        handleMsg state conn
                _ -> return ()
  where
    fromEither (Left err) = error err
    fromEither (Right x)  = x

initialize :: MVar ServerState -> WS.Connection -> IO ()
initialize state conn = do
    st <- readMVar state
    case _current_wd st of
        Nothing -> return ()
        Just wd -> do
            sendResult conn $ CWD wd
            getGraph >>= layoutGraph' >>=
                drawGraph (query (_node_status st)) >>=
                WS.sendBinaryData conn . encode . Raw . encode
            if _is_running st
                then sendResult conn $ Status Running
                else sendResult conn $ Status Stopped
  where
    query status pid = return $ M.findWithDefault Unknown pid status
{-# INLINE initialize #-}

disconnect :: MVar ServerState -> IO ()
disconnect state = modifyMVar_ state $ \x -> x{_main_conn=Nothing}

handleMsg :: MVar ServerState -> WS.Connection -> Command -> IO ()
handleMsg state conn msg = case msg of
    SetCWD wd -> do
        modifyMVar_ state $ \x -> return x{_current_wd = Just wd}
        sendGraph state conn $ T.unpack wd ++ "/sciflow.db"
    Run selected     -> do
        st <- readMVar state
        if _is_running st
            then stopTaiji state (fromJust $ _current_process st) conn
            else runTaiji state selected conn
    Delete selected -> undefined
    _ -> return ()

sendResult :: WS.Connection -> Result -> IO ()
sendResult conn r = do
    traceM $ "Sending: " ++ show r
    WS.sendBinaryData conn $ encode r

sendResult' :: MVar ServerState -> Result -> IO ()
sendResult' state r = do
    conn <- _main_conn <$> readMVar state
    case conn of
        Nothing -> return ()
        Just c -> sendResult c r

sendGraph :: MVar ServerState
          -> WS.Connection
          -> FilePath       -- ^ db file
          -> IO ()
sendGraph state conn db = do
    graph <- getGraph >>= layoutGraph'
    exist <- shelly $ test_f $ fromText $ T.pack db
    graph' <- if exist
        then bracket (openDB db) closeDB $ \h -> drawGraph (readStatus h) graph
        else drawGraph (const (return Unknown)) graph
    WS.sendBinaryData conn $ encode $ Raw $ encode graph'
  where
    readStatus h pid = do
        finished <- isFinished pid h
        let st = if finished then Finished else Unknown
        saveNodeState state pid st
        return st

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
               sendResult' state $ Status Stopped )
    ( \e -> sourceHandle e =$= linesUnboundedAsciiC =$= concatMapMC fn $$
                mapM_C (sendResult' state) )
  where
    cmd = proc "taiji" $ ["run", "--config", "config.yml"] ++ selection
    selection | null selected = []
              | otherwise = ["--select", T.unpack $ T.intercalate "," selected]
    fn x = do
        B.putStrLn x
        processMsg state $ strip x

stopTaiji :: MVar ServerState -> ProcessHandle -> WS.Connection -> IO ()
stopTaiji state p conn = do
    interruptProcessGroupOf p
    modifyMVar_ state $ \x -> return x{_is_running=False}
    sendResult conn $ Status Stopped


processMsg :: MVar ServerState -> B.ByteString -> IO (Maybe Result)
processMsg state msg
    | isMsg msg = Just <$> result
    | isErr msg = return $ Just $ Exception $ T.pack $ B.unpack msg
    | otherwise = return Nothing
  where
    isMsg x = "[LOG][" `B.isPrefixOf` x || "[WARN][" `B.isPrefixOf` x
    isErr = B.isPrefixOf "[ERROR]["
    result = do
        saveNodeState state pid nodeSt
        return $ Notification pid nodeSt
      where
        [field1, field2] = take 2 $ reverse $ B.words msg
        pid = T.init $ T.pack $ B.unpack field2
        nodeSt = case () of
            _ | "running" `B.isPrefixOf` field1 -> InProgress
              | "Failed" `B.isPrefixOf` field1 -> Failed
              | "Finished" `B.isPrefixOf` field1 -> Finished
              | otherwise -> Unknown


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

saveNodeState :: MVar ServerState -> T.Text -> NodeState -> IO ()
saveNodeState state pid nodeSt = modifyMVar_ state $ \x -> do
    let st = M.insert pid nodeSt $ _node_status x
    return x{_node_status=st}
{-# INLINE saveNodeState #-}

strip :: B.ByteString -> B.ByteString
strip = B.pack . reverse . fst . B.foldl' f ([], False)
  where
    f (acc, isESC) x = if isESC
        then if x == 'm' then (acc, False) else (acc, isESC)
        else if x == '\ESC' then (acc, True) else (x:acc, isESC)
{-# INLINE strip #-}
