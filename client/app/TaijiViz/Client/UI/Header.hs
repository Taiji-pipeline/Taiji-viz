{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI.Header
    (header) where

import qualified Data.Vector                   as V
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.SemanticUI.Sidebar

import           TaijiViz.Client.Message
import           TaijiViz.Client.Types
import           TaijiViz.Common.Types

header :: MonadWidget t m
       => m (MenuInput t)
       -> m ()
header content = do
    uiSidebar sidebarOptions sidebarMenu pusher (fmap (const ToggleSidebar))
    return ()
  where
    pusher = do
        rec (evt, menuEvts) <- uiMenu menuInput
            menuInput <- handleMenuEvent menuEvts
            content
        return evt
    sidebarMenu = do
        linkClass "Home" "item"
        linkClass "Topics" "item"
        linkClass "Friends" "item"
        linkClass "History" "item"
    sidebarOptions = def
        { _uiSidebar_closable = False
        , _uiSidebar_dimPage = False
        , _uiSidebar_inverted = Just UiInverted
        , _uiSidebar_className = ["menu", "vertical"]
        }

uiMenu :: MonadWidget t m => MenuInput t -> m (Event t (), MenuEvent t)
uiMenu input = divClass "ui fixed tiny inverted menu" $ do
    link <- linkClass "Menu" "item"

    rec (e, _) <- elClass' "div" "item" $ divClass "ui button" $ do
            let render Nothing = divClass "loader ui active small" (return ())
                render (Just True) = elClass "i" "pause icon" (return ()) >> text "Stop"
                render (Just False) = elClass "i" "play icon" (return ()) >> text "Run"

            evts <- holdDyn (Just False) $ leftmost [const Nothing <$> domEvent Click e, isRunning]
            dyn $ fmap render evts

    return (_link_clicked link, V.fromList [domEvent Click e])
  where
    isRunning = fmapMaybe fn input
        where
          fn (Status Running) = Just (Just True)
          fn (Status _) = Just (Just False)
          fn _ = Nothing

handleMenuEvent :: MonadWidget t m => MenuEvent t -> m (Event t Result)
handleMenuEvent evts = sendMsg $ fmap (const (Run [])) $ evts V.! 0
