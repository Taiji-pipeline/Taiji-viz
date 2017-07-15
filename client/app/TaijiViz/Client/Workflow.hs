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

displayWorkflow :: MonadWidget t m
                => Graph
                -> m (Event t (PID, Attribute))
displayWorkflow Graph{..} = divClass "ui segment" $
    svgAttr "svg"
        [ ("height", T.pack $ show $ max node_h edge_h + 50)
        , ("width", T.pack $ show $ max node_w edge_w + 50)
        ] $ mkEdges edges >> mkNodes nodes
  where
    (node_w, node_h) = (\(x,y) -> (maximum x, maximum y)) $ unzip $
        map nodeCoord nodes
    (edge_w, edge_h) = (\(x,y) -> (maximum x, maximum y)) $ unzip $ concat $
        concat edges

mkNodes :: MonadWidget t m => [Node] -> m (Event t (PID, Attribute))
mkNodes ns = do
    evts <- forM ns $ \Node{..} -> do
        (e, _) <- svgAttr' "rect"
            [ ( "x", T.pack $ show $ fst nodeCoord - nodeWidth / 2)
            , ( "y", T.pack $ show $ snd nodeCoord - 20)
            , ("rx", "6")
            , ("ry", "6")
            , ("width", T.pack $ show nodeWidth)
            , ("height", "30")
            , ("class", getClass nodeState)
            ] $ return ()
        svgAttr "text"
            [ ("x", T.pack $ show $ fst nodeCoord)
            , ("y", T.pack $ show $ snd nodeCoord)
            , ("text-anchor", "middle") ] $ text nodeId
        return $ fmap (const (nodeId, nodeAttr)) $ domEvent Mouseover e
    return $ leftmost evts
  where
    getClass st = case st of
        Finished -> "done"
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
