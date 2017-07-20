{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Server.Workflow where

import           Control.Monad                     (forM)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.ByteString                   as B
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as G
import           Data.Maybe                        (mapMaybe)
import           Data.Serialize                    (decode)
import qualified Data.Text.Lazy                    as TL
import           Scientific.Workflow.Types         (Attribute, PID)
import           Shelly                            hiding (FilePath)

import           TaijiViz.Common.Types

getGraph :: IO (Gr (PID, Attribute) Int)
getGraph = shelly $ silently $ runHandle "taiji" ["view", "--raw"] $ \h -> do
    x <- liftIO $ B.hGetContents h
    case decode x of
        Left err -> error err
        Right r  -> return r

layoutGraph' :: Gr (PID, Attribute) Int
             -> IO (Gr (G.AttributeNode (PID, Attribute)) (G.AttributeEdge Int))
layoutGraph' = G.graphToGraph params
  where
    fmtnode (_, (i, _)) = [G.Label $ G.StrLabel $ TL.fromStrict i]
    params = G.nonClusteredParams
        { G.globalAttributes =
            [ G.NodeAttrs [G.Shape G.BoxShape, G.Margin $ G.DVal 0.2]
            , G.GraphAttrs
                [ G.RankDir G.FromBottom
                , G.Splines G.SplineEdges ]
            ]
        , G.fmtNode = fmtnode
        , G.isDirected = True
        }

drawGraph :: (PID -> IO Bool)
          -> Gr (G.AttributeNode (PID, Attribute)) (G.AttributeEdge Int)
          -> IO Graph
drawGraph isFinished gr = do
    vmap <- forM nodes $ \(_, (attrs, (pid, att))) -> do
        let (pt, width) = getAttr attrs
        finish <- isFinished pid
        return $ Node pid att pt width $ if finish then Finished else Unknown
    return $ Graph vmap edges
  where
    nodes = G.labNodes gr
    getAttr attr = let [p] = [ fromPoint pt | G.Pos (G.PointPos pt) <- attr ]
                       [w] = [ realToFrac (72 * width) | G.Width width <- attr ]
                   in (p, w)
    edges = [ getPath attrs | (_, _, (attrs, _)) <- G.labEdges gr ]

    getPath attrs = head $ flip mapMaybe attrs $ \x -> case x of
        G.Pos (G.SplinePos splines) -> Just $ map getSpline splines
        _                           -> Nothing
    getSpline (G.Spline { G.splinePoints = pts}) = map fromPoint pts
    fromPoint (G.Point x y _ _) = (realToFrac x,realToFrac y)
