{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module TaijiViz.Common.Types where

import           Control.Monad                              (mzero)
import           Data.Aeson                                 (FromJSON (..),
                                                             ToJSON (..),
                                                             Value (..))
import           Data.Binary                                (Binary)
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString.Char8                      as B
import qualified Data.Map.Strict                            as M
import qualified Data.Matrix.Unboxed                        as MU
import qualified Data.Text                                  as T
import           Data.Text.Binary                           ()
import qualified Data.Vector                                as V
import           Data.Vector.Binary                         ()
import           GHC.Generics                               (Generic)
import Taiji.Types (TaijiConfig)
import           Scientific.Workflow.Internal.Builder.Types (Attribute,
                                                             FunctionConfig,
                                                             FunctionType,
                                                             ParallelMode)

type PID = T.Text

-- | Commands that send from client to server.
data Command = SetCWD T.Text
             | Run TaijiConfig [T.Text]
             | Delete [T.Text]
             | Connect
             deriving (Generic)

instance Binary Command

data ProgramStatus = Running
                   | Stopped
                   deriving (Generic, Show)

instance Binary ProgramStatus

data NodeState = Finished
               | Failed
               | InProgress
               | Unknown
               deriving (Generic, Show)

instance Binary NodeState

-- | Results from running the commands, sent by the server to the client.
data Result = Status ProgramStatus          -- ^ program status
            | Gr Graph
            | Notification PID NodeState    -- ^ node status update
            | Exception T.Text              -- ^ error message
            | CWD T.Text                    -- ^ current working directory
            | Config TaijiConfig
    deriving (Generic)

instance Show Result where
    show (Status x) = "Status: " ++ show x
    show (Gr _) = "Graph"
    show (Notification pid st) = "Notification: " ++ show pid ++ " " ++ show st
    show (Exception x) = "Exception: " ++ show x
    show (CWD x) = "CWD: " ++ show x
    show _ = "Something"

instance Binary TaijiConfig
instance Binary Result

type Point = (Double, Double)

type Spline = [Point]

data Node = Node
    { nodeId    :: PID
    , nodeAttr  :: Attribute
    , nodeCoord :: Point         -- ^ unit: point
    , nodeWidth :: Double        -- ^ unit: point
    , nodeState :: NodeState
    } deriving (Generic)

instance Binary FunctionType
instance Binary ParallelMode
instance Binary FunctionConfig
instance Binary Attribute
instance Binary Node

type Edge = [Spline]   -- ^ unit : point

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving (Generic)

instance Binary Graph

data Network = Network
    { tf       :: T.Text
    , parents  :: [ByteString]
    , children :: [ByteString]
    , title    :: T.Text
    } deriving (Generic)

instance ToJSON Network
instance FromJSON Network

instance FromJSON ByteString where
    parseJSON (String x) = return $ B.pack $ T.unpack x
    parseJSON _          = mzero

instance ToJSON ByteString where
    toJSON x = String $ T.pack $ B.unpack x
