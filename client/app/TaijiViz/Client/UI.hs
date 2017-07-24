{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI
    ( ui ) where

import           Reflex.Dom.Core

import TaijiViz.Client.UI.Sidebar (sidebar, CurrentView(..))
import TaijiViz.Client.UI.Home (home)
import TaijiViz.Client.UI.Result (result)

ui :: MonadWidget t m => m ()
ui = do
    view <- sidebar
    container (fmap (==ShowHome) view) home
    container (fmap (==ShowResult) view) result
    return ()


container :: MonadWidget t m => Dynamic t Bool -> m a -> m a
container shouldShow = elDynAttr "div" attr
  where
    attr = flip fmap shouldShow $ \x -> if x
        then [("style", "height:100%")]
        else [("style", "display: none")]
