{-# LANGUAGE DeriveGeneric #-}
module TaijiViz.Common.Types where

import           Data.ByteString           (ByteString)
import           Data.Serialize            (Serialize)
import           Data.Serialize.Text       ()
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Scientific.Workflow.Types (Attribute, PID)

-- | Commands that send from client to server.
data Command = SetCWD T.Text
             | Run [T.Text]
             | Delete [T.Text]
             | Connect
             deriving (Generic)

instance Serialize Command

data ProgramStatus = Running
                   | Stopped
                   deriving (Generic, Show)

instance Serialize ProgramStatus

data NodeState = Finished
               | Failed
               | InProgress
               | Unknown
               deriving (Generic, Show)

instance Serialize NodeState

-- | Results from running the commands, sent by the server to the client.
data Result = Status ProgramStatus
            | Raw ByteString
            | Notification PID NodeState
            | Exception T.Text
    deriving (Generic)

instance Show Result where
    show (Status x) = "Status: " ++ show x
    show (Raw _) = "Raw"
    show (Notification pid st) = "Notification: " ++ show pid ++ " " ++ show st
    show (Exception x) = "Exception: " ++ show x

instance Serialize Result

type Point = (Float, Float)

type Spline = [Point]

data Node = Node
    { nodeId    :: PID
    , nodeAttr  :: Attribute
    , nodeCoord :: Point         -- ^ unit: point
    , nodeWidth :: Float         -- ^ unit: point
    , nodeState :: NodeState
    } deriving (Generic)

instance Serialize Node

type Edge = [Spline]   -- ^ unit : point

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving (Generic)

instance Serialize Graph
