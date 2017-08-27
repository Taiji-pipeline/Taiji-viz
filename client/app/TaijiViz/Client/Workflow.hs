{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.Workflow
    ( displayWorkflow
    , NodeEvents(..)
    ) where

import           Control.Monad
import qualified Data.HashSet                   as S
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import           Reflex.Dom.Contrib.Widgets.Svg (svgAttr, svgDynAttr')
import           Reflex.Dom.Core
import           Scientific.Workflow.Internal.Builder.Types      (Attribute (..))

import           TaijiViz.Common.Types

type NodeStateUpdate t = Event t Result

data NodeEvents t = NodeEvents
    { _node_hover :: Event t (PID, Attribute)
    , _node_click :: Event t PID
    }

displayWorkflow :: MonadWidget t m
                => NodeStateUpdate t
                -> Dynamic t (S.HashSet T.Text)
                -> Graph
                -> m (NodeEvents t)
displayWorkflow update selection Graph{..} = divClass "ui segment" $
    svgAttr "svg"
        [ ("height", T.pack $ show $ max node_h edge_h + 50)
        , ("width", T.pack $ show $ max node_w edge_w + 50)
        , ("class", "workflow")
        ] $ mkEdges edges >> mkNodes (fmapMaybe f update) selection nodes
  where
    (node_w, node_h) = (\(x,y) -> (maximum x, maximum y)) $ unzip $
        map (\Node{..} -> (\(a,b) -> (a + nodeWidth / 2, b)) nodeCoord) nodes
    (edge_w, edge_h) = (\(x,y) -> (maximum x, maximum y)) $ unzip $ concat $
        concat edges
    f (Notification pid st) = Just (pid, st)
    f _                     = Nothing

mkNodes :: MonadWidget t m
        => Event t (PID, NodeState)
        -> Dynamic t (S.HashSet T.Text)
        -> [Node]
        -> m (NodeEvents t)
mkNodes update selection ns = do
    (evts1, evts2) <- fmap unzip $ forM ns $ \Node{..} -> do
        statusUpdate <- holdDyn nodeState $ fmap snd $
            ffilter ((==nodeId) . fst) update
        let baseAttr =
                [ ( "x", T.pack $ show $ fst nodeCoord - nodeWidth / 2)
                , ( "y", T.pack $ show $ snd nodeCoord - 20)
                , ("rx", "6")
                , ("ry", "6")
                , ("width", T.pack $ show nodeWidth)
                , ("height", "30") ] :: [(T.Text, T.Text)]
            fn st slct =
                let cls = T.unwords [st', isSelected]
                    isSelected | nodeId `S.member` slct = "select"
                               | otherwise = ""
                    st' = case st of
                        Finished   -> "done"
                        InProgress -> "progress"
                        Failed     -> "fail"
                        Unknown    -> "await"
                in M.fromList $ ("class", cls) : baseAttr
            attr = zipDynWith fn statusUpdate selection
        (e, _) <- svgDynAttr' "rect" attr $ return ()

        svgAttr "text"
            [ ("x", T.pack $ show $ fst nodeCoord)
            , ("y", T.pack $ show $ snd nodeCoord)
            , ("text-anchor", "middle")
            , ("z-index", "9") ] $ text nodeId

        return ( const (nodeId, nodeAttr) <$> domEvent Mouseover e
               , const nodeId <$> domEvent Click e )

    return $ NodeEvents (leftmost evts1) $ leftmost evts2
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
mkSpline _ = return ()
{-# INLINE mkSpline #-}
