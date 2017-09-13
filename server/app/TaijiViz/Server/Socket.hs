{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Server.Socket
    ( ServerState(..)
    , defaultServerState
    , socketApp
    , readLog
    ) where

import           Conduit
import           Control.Arrow                      ((&&&))
import           Control.Concurrent                 (MVar, forkIO, modifyMVar_,
                                                     readMVar)
import           Control.Exception                  (bracket, finally)
import           Control.Monad                      (forM_, forever)
import           Data.Binary                        (decode, encode)
import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString.Lazy               as BL
import           Data.Default                       (def)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromJust, fromMaybe)
import qualified Data.Serialize                     as S
import qualified Data.Text                          as T
import           Data.Yaml                          (decodeFile, encodeFile)
import           Network.Socket                     hiding (recv)
import           Network.Socket.ByteString
import qualified Network.WebSockets                 as WS
import           Scientific.Workflow.Internal.DB    (closeDB, isFinished,
                                                     openDB)
import qualified Scientific.Workflow.Internal.Utils as W
import           Shelly                             (chdir, fromText, run_,
                                                     shelly, test_f)
import           System.Process                     (CreateProcess (..),
                                                     ProcessHandle,
                                                     StdStream (..),
                                                     createProcess,
                                                     interruptProcessGroupOf,
                                                     proc)
import           Taiji.Types                        (TaijiConfig)

import           TaijiViz.Common.Types
import           Taiji.Types (TaijiResults(..))
import           TaijiViz.Server.Workflow

import           Debug.Trace

unixSocketAddr :: String
unixSocketAddr = "Taiji.socket"

configFileName :: String
configFileName = "config.yml"

data ServerState = ServerState
    { _current_process :: Maybe ProcessHandle
    , _current_wd      :: Maybe T.Text
    , _is_running      :: Bool
    , _node_status     :: M.Map T.Text NodeState
    , _main_conn       :: Maybe WS.Connection
    , _final_result    :: Maybe TaijiResults
    }

defaultServerState :: ServerState
defaultServerState = ServerState
    { _current_process = Nothing
    , _current_wd      = Nothing
    , _is_running      = False
    , _node_status = M.empty
    , _main_conn = Nothing
    , _final_result = Nothing
    }

socketApp :: MVar ServerState -> WS.ServerApp
socketApp state pending = do
    currentConn <- _main_conn <$> readMVar state
    case currentConn of
        Just _ -> WS.rejectRequest pending "Can only have 1 connection at a time"
        Nothing -> do
            conn <- WS.acceptRequest pending
            WS.forkPingThread conn 30
            msg <- decode . BL.fromStrict <$> WS.receiveData conn
            case msg of
                Connect -> flip finally (disconnect state) $ do
                    modifyMVar_ state $ \x -> return x{_main_conn=Just conn}
                    initialize state conn
                    forever $ decode . BL.fromStrict <$> WS.receiveData conn >>=
                        handleMsg state conn
                _ -> sendResult conn $ Exception "Invalid request"


readLog :: MVar ServerState -> IO ()
readLog state = do
    sock <- socket AF_UNIX Stream defaultProtocol
    bind sock $ SockAddrUnix ('\0' : unixSocketAddr)
    listen sock 1
    forever $ do
        (s, _) <- accept sock
        loop s
  where
    loop s = do
        d <- getData s
        case d of
            W.Exit -> putStrLn "disconnected" >> close s
            W.Running pid -> do
                saveNodeState state pid InProgress
                sendResult' state $ Notification pid InProgress
                loop s
            W.Complete pid -> do
                saveNodeState state pid Finished
                sendResult' state $ Notification pid Finished
                loop s
            W.Warn pid msg -> do
                saveNodeState state pid Failed
                sendResult' state $ Notification pid Failed
                loop s
            W.Error msg -> do
                sendResult' state $ Exception $ T.pack msg
                loop s
    getData s = do
        h <- S.decode <$> recv s 4096
        case h of
            Right x -> return x
            Left e  -> putStrLn e >> return W.Exit

initialize :: MVar ServerState -> WS.Connection -> IO ()
initialize state conn = do
    st <- readMVar state
    case _current_wd st of
        Nothing -> return ()
        Just wd -> do
            sendResult conn $ CWD wd
            getGraph >>= layoutGraph' >>=
                drawGraph (query (_node_status st)) >>=
                sendResult conn . Gr
            sendConfig (T.unpack wd) conn
            if _is_running st
                then sendResult conn $ Status Running
                else sendResult conn $ Status Stopped
  where
    query status pid = return $ M.findWithDefault Unknown pid status
{-# INLINE initialize #-}

disconnect :: MVar ServerState -> IO ()
disconnect state = modifyMVar_ state $ \x -> return x{_main_conn=Nothing}

sendConfig :: FilePath -> WS.Connection -> IO ()
sendConfig dir conn = do
    config <- readConfig dir
    sendResult conn $ Config $ fromMaybe def config

readConfig :: FilePath -> IO (Maybe TaijiConfig)
readConfig dir = do
    let fl = dir ++ "/" ++ configFileName
    exist <- shelly $ test_f $ fromText $ T.pack fl
    if exist
        then decodeFile fl
        else return Nothing

handleMsg :: MVar ServerState -> WS.Connection -> Command -> IO ()
handleMsg state conn msg = case msg of
    SetCWD wd -> do
        modifyMVar_ state $ \x -> return x{_current_wd = Just wd}
        sendGraph state conn $ T.unpack wd ++ "/sciflow.db"
        sendConfig (T.unpack wd) conn
    Run config selected -> do
        st <- readMVar state
        if _is_running st
            then stopTaiji state (fromJust $ _current_process st) conn
            else forkIO (runTaiji state config selected conn) >> return ()
    Delete selected -> do
        wd <- _current_wd <$> readMVar state
        forM_ selected $ \pid -> do
            shelly $ chdir (fromText $ fromJust wd) $ run_ "taiji" ["rm", pid]
            modifyMVar_ state $ \x -> return
                x{ _node_status = M.delete pid (_node_status x)}
            sendResult conn $ Notification pid Unknown
    _ -> return ()

sendResult :: WS.Connection -> Result -> IO ()
sendResult conn r = do
    traceM $ "Sending: " ++ show r
    WS.sendBinaryData conn $ BL.toStrict $ encode r

sendResult' :: MVar ServerState -> Result -> IO ()
sendResult' state r = do
    conn <- _main_conn <$> readMVar state
    case conn of
        Nothing -> return ()
        Just c  -> sendResult c r

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
    modifyMVar_ state $ \st -> return st{_node_status =
        M.fromList $ map (nodeId &&& nodeState) $ nodes graph'}
    sendResult conn $ Gr graph'
  where
    readStatus h pid = do
        finished <- isFinished pid h
        let st = if finished then Finished else Unknown
        saveNodeState state pid st
        return st

runTaiji :: MVar ServerState -> TaijiConfig -> [T.Text] -> WS.Connection -> IO ()
runTaiji state config selected conn = bracket
    ( do wd <- _current_wd <$> readMVar state
         encodeFile (T.unpack (fromMaybe "" wd) ++ "/" ++ configFileName) config
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
    ( \e -> sourceHandle e =$= linesUnboundedAsciiC $$ mapM_C fn )
  where
    cmd = proc "taiji" $ ["run", "--log-server", "\\0" ++ unixSocketAddr
        , "--config", configFileName] ++ selection
    selection | null selected = []
              | otherwise = ["--select", T.unpack $ T.intercalate "," selected]
    fn x = do
        B.putStrLn x

stopTaiji :: MVar ServerState -> ProcessHandle -> WS.Connection -> IO ()
stopTaiji state p conn = do
    interruptProcessGroupOf p
    modifyMVar_ state $ \x -> return x{_is_running=False}
    sendResult conn $ Status Stopped

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
