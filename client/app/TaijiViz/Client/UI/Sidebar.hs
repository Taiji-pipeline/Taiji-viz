{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecursiveDo           #-}
module TaijiViz.Client.UI.Sidebar
    (CurrentView(..)
    , sidebar) where

import qualified Data.Text as T
import           Reflex.Dom.Core

data CurrentView = ShowHome
                 | ShowResult
                 deriving (Eq)

sidebar :: MonadWidget t m => m (Dynamic t CurrentView)
sidebar = elAttr "div"
    [ ("style", "position: fixed; bottom: 20px; right: 20px; z-index:999;")
    , ("class", "toolbar")
    ] $ do
        rec (e1, _) <- elDynClass' "i"
                (getClass ("home":iconClass) ShowHome views) $ return ()
            (e2, _) <- elDynClass' "i"
                (getClass ("unhide":iconClass) ShowResult views) $ return ()
            views <- holdDyn ShowHome $ leftmost
                [ const ShowHome <$> domEvent Click e1
                , const ShowResult <$> domEvent Click e2 ]
        return views
  where
    getClass baseClass subscr = fmap (f . (==subscr))
      where
        f True = T.unwords $ "large" : baseClass
        f False = T.unwords baseClass
    iconClass = ["link", "inverted", "teal", "circular", "icon"]
