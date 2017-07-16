{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI.Header
    ( MenuEvent(..)
    , header
    ) where

import qualified Data.Vector                   as V
import qualified Data.Text as T
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.SemanticUI.Sidebar

import           TaijiViz.Client.Message
import           TaijiViz.Client.Types
import           TaijiViz.Common.Types

data MenuEvent t = MenuEvent
    { _menu_toggle :: Event t ()
    , _menu_run :: Event t ()
    , _menu_cwd :: Dynamic t T.Text
    , _menu_set_cwd :: Event t ()
    }

header :: MonadWidget t m
       => ((Event t Result, MenuEvent t) -> m (Event t Result))
       -> m ()
header content = do
    uiSidebar sidebarOptions sidebarMenu pusher (fmap (const ToggleSidebar))
    return ()
  where
    pusher = do
        rec menuEvts <- uiMenu menuInput
            menuInput <- handleMenuRun menuEvts
            content (menuInput, menuEvts)
        return $ _menu_toggle menuEvts
    sidebarMenu = do
        linkClass "Home" "item"
        linkClass "Topics" "item"
        linkClass "History" "item"
    sidebarOptions = def
        { _uiSidebar_closable = False
        , _uiSidebar_dimPage = False
        , _uiSidebar_inverted = Just UiInverted
        , _uiSidebar_className = ["menu", "vertical"]
        }

uiMenu :: MonadWidget t m => MenuInput t -> m (MenuEvent t)
uiMenu clickRunResult = divClass "ui fixed inverted menu" $ do
    link <- linkClass "Menu" "item"

    rec (e, _) <- elClass' "div" "item" $ do
            evts <- holdDyn (Just False) $ mergeWith combine
                [const Nothing <$> domEvent Click e, isRunning]
            dyn $ flip fmap evts $ \x -> case x of
                Nothing -> elClass "button" "ui loading button" $ text "Loading"
                Just False -> elClass "button" "ui icon labeled button" $ do
                    elClass "i" "play icon" $ return ()
                    text "Run"
                Just True -> elClass "button" "ui icon labeled button" $ do
                    elClass "i" "pause icon" $ return ()
                    text "Stop"

    (txt, c) <- divClass "item" $ divClass "ui action labeled input" $ do
        divClass "ui label" $ text "working directory:"
        txt <- textInput def
        (e, _) <- elClass' "button" "ui compact button" $ text "Refresh"
        return (_textInput_value txt, domEvent Click e)

    return $ MenuEvent (_link_clicked link) (domEvent Click e) txt c
  where
    isRunning = fmapMaybe fn clickRunResult
        where
          fn (Status Running) = Just (Just True)
          fn (Status _) = Just (Just False)
          fn (Exception _) = Just (Just False)
          fn _ = Nothing
    combine Nothing x = x
    combine x Nothing = x
    combine x y = x

handleMenuRun :: MonadWidget t m
              => MenuEvent t
              -> m (Event t Result)
handleMenuRun MenuEvent{..} = do
    let evt = tag (current _menu_cwd) _menu_run
    result <- sendMsg $ const (Run []) <$> ffilter (not . T.null) evt
    return $ leftmost [ result
        , const (Exception "Please set working directory first") <$>
        ffilter T.null evt ]
