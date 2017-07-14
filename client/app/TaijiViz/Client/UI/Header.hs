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
            menuInput <- content
        handleMenuEvent menuEvts
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
            let t0 = elClass "i" "play icon" (return ()) >> text "Run"
                t = elClass "i" "pause icon" (return ()) >> text "Stop"
            clk <- foldDyn (\_ x -> not x) True $ domEvent Click e
            dyn $ (\x -> if x then t0 else t) <$> clk

    return (_link_clicked link, V.fromList [domEvent Click e])


handleMenuEvent :: MonadWidget t m => MenuEvent t -> m ()
handleMenuEvent evts = do
    sendMsg $ fmap (const (Run [])) $ evts V.! 0
    return ()
