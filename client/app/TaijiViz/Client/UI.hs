{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI
    ( MenuEvent(..)
    , header
    ) where

import Control.Monad
import qualified Data.Vector                   as V
import qualified Data.Text as T
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.SemanticUI.Sidebar
import qualified Data.HashSet as S

import           TaijiViz.Client.Message
import           TaijiViz.Client.Workflow (NodeEvents(..))
import           TaijiViz.Client.Types
import           TaijiViz.Common.Types

data MenuEvent t = MenuEvent
    { _menu_toggle :: Event t ()
    , _menu_run :: Event t (Maybe Result)
    , _menu_set_cwd :: Event t Result
    , _menu_delete :: Event t ()
    }

header :: MonadWidget t m
       => ( MenuEvent t -> m (Event t (NodeEvents t)) )
       -> m ()
header content = do
    uiSidebar sidebarOptions sidebarMenu pusher (fmap (const ToggleSidebar))
    return ()
  where
    pusher = do
        rec menuEvts <- menu nodeEvts
            nodeEvts <- content menuEvts
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

menu :: MonadWidget t m
     => Event t (NodeEvents t)
     -> m (MenuEvent t)
menu nodeEvts = divClass "ui fixed inverted menu" $ do
    selection <- handleNodeClickEvent nodeEvts

    link <- linkClass "Menu" "item"

    rec (cwdEvts, canSetWD) <- handleMenuSetWD runEvts (c, txt)
        (runEvts, canRun) <- handleMenuRun selection cwdEvts $
            domEvent Click runButton
        canDelete <- handleDelete selection runEvts

        -- Working directory
        (txt, c) <- divClass "item" $ divClass "ui action labeled input" $ do
            divClass "ui label" $ text "working directory:"
            txt <- textInput def
            dynClass <- flip mapDyn canSetWD $ \x -> case x of
                True -> "ui button"
                False -> "ui button disabled"
            (e, _) <- elDynClass' "button" dynClass $ text "Refresh"
            return (_textInput_value txt, domEvent Click e)

        -- Run botton
        runStatus <- holdDyn (Just False) $ fmap isRunning runEvts
        (runButton, _) <- elClass' "div" "item" $ dyn $ zipDynWith f canRun runStatus

        -- Delete button
        (delButton, _) <- elClass' "div" "item" $ do
            dynClass <- flip mapDyn canDelete $ \x -> case x of
                True -> "ui button negative"
                False -> "ui button negative disabled"
            elDynClass "button" dynClass $ text "Delete"

    return $ MenuEvent (_link_clicked link) runEvts cwdEvts
        (domEvent Click delButton)
  where
    isRunning x = case x of
        Just (Status Running) -> Just True
        Just (Status _) -> Just False
        Just (Exception _) -> Just False
        Nothing -> Nothing
    f a b = case a of
        False -> elClass "button" "disabled ui icon labeled button" $ do
            elClass "i" "play icon" $ return ()
            text "Run"
        True -> case b of
            Nothing -> elClass "button" "ui loading button" $ text "Loading"
            Just False -> elClass "button" "ui positive icon labeled button" $ do
                elClass "i" "play icon" $ return ()
                text "Run"
            Just True -> elClass "button" "ui negative icon labeled button" $ do
                elClass "i" "pause icon" $ return ()
                text "Stop"

-- | "Run" button handler.
-- Nothing: waiting for result
-- Just _ : result is available
handleMenuRun :: MonadWidget t m
              => Dynamic t (S.HashSet T.Text)
              -> Event t Result   -- set CWD event
              -> Event t ()
              -> m (Event t (Maybe Result), Dynamic t Bool)
handleMenuRun clkNode set_cwd menu_run = do
    isSet <- holdDyn False $ fmap g set_cwd
    let runEvt = attachDyn clkNode $ tag (current isSet) menu_run
    result <- sendMsg $ fn <$> ffilter snd runEvt
    return (leftmost [Just <$> result, const Nothing <$> runEvt], isSet)
  where
    fn (pids, _) | S.null pids = Run []
                 | otherwise = Run $ S.toList pids
    g (Exception _) = False
    g _ = True

-- | "Set CWD" handler
handleMenuSetWD :: MonadWidget t m
                => Event t (Maybe Result)   -- Run event
                -> (Event t (), Dynamic t T.Text)
                -> m (Event t Result, Dynamic t Bool)
handleMenuSetWD runEvts (set_cwd, cwd) = do
    isAvailable <- holdDyn True $ fmap fn runEvts
    response <- sendMsg $ fmap SetCWD $ tag (current cwd) $ ffilter id $
        tag (current isAvailable) set_cwd
    return (response, isAvailable)
  where
    fn (Just (Status Running)) = False
    fn Nothing = False
    fn _ = True

handleDelete :: MonadWidget t m
             => Dynamic t (S.HashSet T.Text)
             -> Event t (Maybe Result)
             -> m (Dynamic t Bool)
handleDelete selection runEvts = do
    a <- holdDyn True $ fmap fn runEvts
    let isAvailable = zipDynWith (&&) a $ fmap (not . S.null) selection
    return isAvailable
  where
    fn (Just (Status Running)) = False
    fn Nothing = False
    fn _ = True

handleNodeClickEvent :: MonadWidget t m
                     => Event t (NodeEvents t) -> m (Dynamic t (S.HashSet T.Text))
handleNodeClickEvent evts = do
    flatEvt <- switchPromptOnly never $ _node_click <$> evts
    let evts' = leftmost [ const Nothing <$> evts, Just <$> flatEvt ]
    foldDyn f S.empty evts'
  where
    f Nothing _ = S.empty
    f (Just pid) set = if S.member pid set
        then S.delete pid set
        else S.insert pid set
