{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module TaijiViz.Client.Workflow
    ( displayWorkflow
    , nodeInfo
    ) where

import           Control.Monad
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           Reflex.Dom.Contrib.Widgets.Svg (svg, svgAttr, svgAttr')
import           Reflex.Dom.Core
import           Scientific.Workflow.Types      (Attribute (..), PID)

import           TaijiViz.Common.Types

type NodeStateUpdate t = Event t Result

displayWorkflow :: MonadWidget t m
                => NodeStateUpdate t
                -> Graph
                -> m (Event t (PID, Attribute))
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
    f _ = Nothing

mkNodes :: MonadWidget t m
        => Event t (PID, NodeState)
        -> [Node]
        -> m (Event t (PID, Attribute))
mkNodes update ns = do
    evts <- forM ns $ \Node{..} -> do
        beh <- holdDyn nodeState $ fmap snd $ ffilter ((==nodeId) . fst) update
        mouseEvt <- dyn $ flip fmap beh $ \st -> do
            (e, _) <- svgAttr' "rect"
                [ ( "x", T.pack $ show $ fst nodeCoord - nodeWidth / 2)
                , ( "y", T.pack $ show $ snd nodeCoord - 20)
                , ("rx", "6")
                , ("ry", "6")
                , ("width", T.pack $ show nodeWidth)
                , ("height", "30")
                , ("class", getClass st)
                ] $ return ()
            return $ fmap (const (nodeId, nodeAttr)) $ domEvent Mouseover e
        svgAttr "text"
            [ ("x", T.pack $ show $ fst nodeCoord)
            , ("y", T.pack $ show $ snd nodeCoord)
            , ("text-anchor", "middle") ] $ text nodeId
        switchPromptly never mouseEvt
    return $ leftmost evts
  where
    getClass st = case st of
        Finished -> "done"
        InProgress -> "progress"
        Failed -> "fail"
        Unknown -> "await"
        _ -> "await"
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
