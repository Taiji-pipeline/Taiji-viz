{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
module TaijiViz.Client.UI.Sidebar
    (sidebar) where

import           Reflex.Dom.Core

sidebar :: MonadWidget t m => m ()
sidebar = elAttr "div"
    [ ("style", "position: fixed; bottom: 20px; right: 20px; z-index:999;")
    ] $ do
        elClass "i" "large inverted teal circular home icon" $ return ()
        elClass "i" "large inverted teal circular unhide icon" $ return ()
        return ()
