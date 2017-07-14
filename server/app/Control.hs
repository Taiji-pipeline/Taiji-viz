{-# LANGUAGE OverloadedStrings #-}
module Control where

import qualified Data.ByteString as B
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import           Data.GraphViz.Commands.IO         (hGetDot)
import           Data.GraphViz.Types.Generalised   (FromGeneralisedDot (..))
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust, mapMaybe)
import           Data.Serialize                    (decode)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Scientific.Workflow.Types
import           Shelly                            hiding (FilePath)


getGraph :: IO (Gr (PID, Attribute) Int)
getGraph = do
    shelly $ silently $ escaping False $ run_ "taiji" ["view", "--raw", ">", "tmp"]
    f <- B.readFile "tmp"
    case decode f of
        Left err -> error err
        Right r  -> return r

layoutGraph' :: Gr (PID, Attribute) Int
             -> IO (Gr (G.AttributeNode (PID, Attribute)) (G.AttributeEdge Int))
layoutGraph' = G.graphToGraph params
  where
    --asDot = G.graphToDot params gr'
    fmtnode (_, (i, attr)) = [G.Label $ G.StrLabel $ TL.fromStrict i]
    params = G.nonClusteredParams
        { G.globalAttributes =
            [ G.NodeAttrs [G.Shape G.BoxShape]
            , G.GraphAttrs [G.RankDir G.FromBottom , G.Splines G.SplineEdges]
            ]
        , G.fmtNode = fmtnode
        , G.isDirected = True
        }
    --gr' = G.addEdgeIDs gr

type Point = (Float, Float)

drawGraph :: Gr (G.AttributeNode (PID, Attribute)) (G.AttributeEdge Int)
         ->  ( [(PID, (Attribute, Point))], [[[Point]]] )
drawGraph gr = (vmap, edges)
  where
    nodes = G.labNodes gr
    vmap = [ (pid, (att, fromPoint pt)) | (_, (attrs, (pid, att))) <- nodes
        , G.Pos (G.PointPos pt) <- attrs ]
    {-
    ixmap = M.fromList [ (i, pid) | (i, (_, (pid, _))) <- nodes ]
    -}

    edges = [ getPath attrs | (i, j, (attrs, _)) <- G.labEdges gr ]

    getPath attrs = head $ flip mapMaybe attrs $ \x -> case x of
        G.Pos (G.SplinePos splines) -> Just $ map getSpline splines
        _ -> Nothing

    getSpline (G.Spline { G.splinePoints = pts}) = map fromPoint pts
    getSpline _ = error "Don't know what to do with empty spline!"
    fromPoint (G.Point x y _ _) = (realToFrac x,realToFrac y)
