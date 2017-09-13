{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI
    ( ui ) where

import           Reflex.Dom.Core

import           TaijiViz.Client.UI.Home         (home)
import           TaijiViz.Client.UI.MessageBoard
import           TaijiViz.Client.UI.Result       (result)
import           TaijiViz.Client.UI.Sidebar      (CurrentView (..), sidebar)

ui :: MonadWidget t m => m ()
ui = do
    view <- sidebar
    response <- container (fmap (==ShowHome) view) home
    container (fmap (==ShowResult) view) result
    elAttr "div" [("style", "position:fixed;bottom:5px;left:5px;z-index:999;opacity:0.9")] $ messageBoard response
    return ()

container :: MonadWidget t m => Dynamic t Bool -> m a -> m a
container shouldShow = elDynAttr "div" attr
  where
    attr = flip fmap shouldShow $ \x -> if x
        then [("style", "height:100%")]
        else [("style", "display: none")]
