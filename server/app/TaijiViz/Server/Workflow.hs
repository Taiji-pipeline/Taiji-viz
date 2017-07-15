{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Server.Workflow where

import           Control.Monad                     (forM)
import qualified Data.ByteString                   as B
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
import           Scientific.Workflow.DB            (closeDB, isFinished, openDB)
import           Scientific.Workflow.Types         (Attribute, PID)
import           Shelly                            hiding (FilePath)

import           Debug.Trace
import           TaijiViz.Common.Types


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
    fmtnode (_, (i, attr)) = [G.Label $ G.StrLabel $ TL.fromStrict i]
    params = G.nonClusteredParams
        { G.globalAttributes =
            [ G.NodeAttrs [G.Shape G.BoxShape]
            , G.GraphAttrs [G.RankDir G.FromBottom , G.Splines G.SplineEdges]
            ]
        , G.fmtNode = fmtnode
        , G.isDirected = True
        }

drawGraph :: FilePath  -- ^ db file
          -> Gr (G.AttributeNode (PID, Attribute)) (G.AttributeEdge Int)
          -> IO Graph
drawGraph db gr = do
    exist <- shelly $ test_f $ fromText $ T.pack db
    vmap <- if exist
        then do
            db' <- openDB db
            result <- forM nodes $ \(_, (attrs, (pid, att))) -> do
                let (pt, width) = getAttr attrs
                finish <- isFinished pid db'
                return $ Node pid att pt width $ if finish then Finished else Unknown
            closeDB db'
            return result
        else do
            forM nodes $ \(_, (attrs, (pid, att))) -> do
                let (pt, width) = getAttr attrs
                return $ Node pid att pt width Unknown
    return $ Graph vmap edges
  where
    nodes = G.labNodes gr
    getAttr attr = let [p] = [ fromPoint pt | G.Pos (G.PointPos pt) <- attr ]
                       [w] = [ realToFrac (72 * width) | G.Width width <- attr ]
                   in (p, w)
    edges = [ getPath attrs | (i, j, (attrs, _)) <- G.labEdges gr ]

    getPath attrs = head $ flip mapMaybe attrs $ \x -> case x of
        G.Pos (G.SplinePos splines) -> Just $ map getSpline splines
        _                           -> Nothing
    getSpline (G.Spline { G.splinePoints = pts}) = map fromPoint pts
    getSpline _ = error "Don't know what to do with empty spline!"
    fromPoint (G.Point x y _ _) = (realToFrac x,realToFrac y)
