{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

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

type Point = (Float, Float)

displayWorkflow :: MonadWidget t m
                => Either String ( [(PID, (Attribute, Point))], [[[Point]]] )
                -> m (Event t (PID, Attribute))
displayWorkflow (Left err) = do
    el "div" $ text $ T.pack err
    return never
displayWorkflow (Right (nodes, edges)) = divClass "ui segment" $
    svgAttr "svg" [ ("height", "2000px"), ("width", "1000px")] $ do
        mkEdges edges
        mkNodes nodes

mkNodes :: MonadWidget t m => [(PID, (Attribute, Point))] -> m (Event t (PID, Attribute))
mkNodes ns = do
    evts <- forM ns $ \(pid, (attr, (x, y))) -> do
        (e, _) <- svgAttr' "text"
            [ ("x", T.pack $ show x)
            , ("y", T.pack $ show y)
            , ("text-anchor", "middle") ] $ text pid
        return $ fmap (const (pid,attr)) $ domEvent Mouseover e
    return $ leftmost evts
{-# INLINE mkNodes #-}

mkEdges :: MonadWidget t m => [[[Point]]] -> m ()
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
