{-# LANGUAGE DeriveGeneric #-}
module TaijiViz.Common.Types where

import           Data.ByteString           (ByteString)
import           Data.Binary (Binary)
import           Data.Text.Binary ()
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Scientific.Workflow.Types (Attribute, PID)

-- | Commands that send from client to server.
data Command = SetCWD T.Text
             | Run [T.Text]
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
            | Gr Graph                -- ^ some data
            | Notification PID NodeState    -- ^ node status update
            | Exception T.Text              -- ^ error message
            | CWD T.Text                    -- ^ current working directory
    deriving (Generic)

instance Show Result where
    show (Status x) = "Status: " ++ show x
    show (Gr _) = "Graph"
    show (Notification pid st) = "Notification: " ++ show pid ++ " " ++ show st
    show (Exception x) = "Exception: " ++ show x
    show (CWD x) = "CWD: " ++ show x

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

instance Binary Attribute
instance Binary Node

type Edge = [Spline]   -- ^ unit : point

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving (Generic)

instance Binary Graph
