{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo #-}

module TaijiViz.Client.Workflow
    ( displayWorkflow
    , nodeInfo
    , NodeEvents(..)
    ) where

import           Control.Monad
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           Reflex.Dom.Contrib.Widgets.Svg (svg, svgAttr, svgDynAttr', svgAttr')
import           Reflex.Dom.Core
import           Scientific.Workflow.Types      (Attribute (..), PID)

import           TaijiViz.Common.Types

type NodeStateUpdate t = Event t Result

data NodeEvents t = NodeEvents
    { nodeHover :: Event t (PID, Attribute)
    , nodeClick :: Event t PID
    }

displayWorkflow :: MonadWidget t m
                => NodeStateUpdate t
                -> Graph
                -> m (NodeEvents t)
displayWorkflow update Graph{..} = divClass "ui segment" $
    svgAttr "svg"
        [ ("height", T.pack $ show $ max node_h edge_h + 50)
        , ("width", T.pack $ show $ max node_w edge_w + 50)
        ] $ mkEdges edges >> mkNodes (fmapMaybe f update) nodes
  where
    (node_w, node_h) = (\(x,y) -> (maximum x, maximum y)) $ unzip $
        map nodeCoord nodes
    (edge_w, edge_h) = (\(x,y) -> (maximum x, maximum y)) $ unzip $ concat $
        concat edges
    f (Notification pid st) = Just (pid, st)
    f _                     = Nothing

mkNodes :: MonadWidget t m
        => Event t (PID, NodeState)
        -> [Node]
        -> m (NodeEvents t)
mkNodes update ns = do
    (evts1, evts2) <- fmap unzip $ forM ns $ \Node{..} -> do
        beh <- holdDyn nodeState $ fmap snd $ ffilter ((==nodeId) . fst) update
        mouseEvt <- dyn $ flip fmap beh $ \st -> do
            rec (e, _) <- do
                    clk <- foldDyn (\_ x -> not x) False $ domEvent Click e
                    let attr = flip fmap clk $ \c ->
                            [ ( "x", T.pack $ show $ fst nodeCoord - nodeWidth / 2)
                            , ( "y", T.pack $ show $ snd nodeCoord - 20)
                            , ("rx", "6")
                            , ("ry", "6")
                            , ("width", T.pack $ show nodeWidth)
                            , ("height", "30")
                            , ("class", getClass st c)
                            ]
                    svgDynAttr' "rect" attr $ return ()
            return ( const (nodeId, nodeAttr) <$> domEvent Mouseover e
                   , const nodeId <$> domEvent Click e )
        svgAttr "text"
            [ ("x", T.pack $ show $ fst nodeCoord)
            , ("y", T.pack $ show $ snd nodeCoord)
            , ("text-anchor", "middle") ] $ text nodeId
        evt1 <- switchPromptly never $ fmap fst mouseEvt
        evt2 <- switchPromptly never $ fmap snd mouseEvt
        return (evt1, evt2)
    return $ NodeEvents (leftmost evts1) $ leftmost evts2
  where
    getClass st x = T.unwords [c1,c2]
      where
        c1 = case st of
            Finished   -> "done"
            InProgress -> "progress"
            Failed     -> "fail"
            Unknown    -> "await"
            _          -> "await"
        c2 | x = "select"
           | otherwise = ""
{-# INLINE mkNodes #-}

mkEdges :: MonadWidget t m => [Edge] -> m ()
mkEdges es = forM_ (concat es) mkSpline
{-# INLINE mkEdges #-}

mkSpline :: MonadWidget t m => [Point] -> m ()
mkSpline ((x1,x2):pts) = svgAttr "path"
    [ ("d", spline)
    , ("stroke", "black")
    , ("fill", "transparent")
    ] $ return ()
  where
    spline = T.unwords ["M", T.pack $ show x1, T.pack $ show x2] `T.append`
        " " `T.append` "C " `T.append` T.intercalate ", "
        (flip map pts $ \(x,y) -> T.unwords [T.pack $ show x, T.pack $ show y])
{-# INLINE mkSpline #-}

nodeInfo :: MonadWidget t m => Dynamic t (Maybe (PID, Attribute)) -> m ()
nodeInfo x = divClass "info-bar" $
    divClass "ui card" $ divClass "content" $ do
        divClass "header" $ dyn $ fmap (text . getText fst) x
        divClass "description" $ el "p" $ dyn $ fmap (text . getText (_note . snd)) x
        return ()
  where
    getText f = fromMaybe "" . fmap f
