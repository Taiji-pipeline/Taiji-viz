{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI
    ( MenuEvent(..)
    , header
    , handleNodeClickEvent
    ) where

import           Control.Monad
import qualified Data.HashSet                  as S
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Reflex.Dom.Core               hiding (Delete)
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.SemanticUI.Sidebar

import           TaijiViz.Client.Message
import           TaijiViz.Client.Types
import           TaijiViz.Client.Workflow      (NodeEvents (..))
import           TaijiViz.Common.Types

data MenuEvent t = MenuEvent
    { _menu_toggle  :: Event t ()
    , _menu_run     :: Event t Command
    , _menu_set_cwd :: Event t Command
    , _menu_delete  :: Event t Command
    }

header :: MonadWidget t m
       => ServerResponse t
       -> m (Event t (NodeEvents t))
       -> m (MenuEvent t)
header response content = fmap snd $ uiSidebar sidebarOptions sidebarMenu
    pusher (fmap (const ToggleSidebar) . _menu_toggle)
  where
    pusher = do
        rec menuEvts <- menu response nodeEvts
            nodeEvts <- content
        return menuEvts
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
     => ServerResponse t
     -> Event t (NodeEvents t)
     -> m (MenuEvent t)
menu response@(ServerResponse r) nodeEvts = divClass "ui fixed inverted menu" $ do
    selection <- handleNodeClickEvent nodeEvts

    link <- linkClass "Menu" "item"

    rec (runEvts, runBtnSt) <- handleMenuRun response selection $
            domEvent Click runButton
        let (cwdEvts, canSetWD) = handleMenuSetWD runBtnSt (c, txt)
            (delEvts, canDelete) = handleDelete selection runBtnSt $
                domEvent Click delButton

        -- Working directory
        (txt, c) <- divClass "item" $ divClass "ui action labeled input" $ do
            divClass "ui label" $ text "working directory:"
            let setTxt = flip fmapMaybe r $ \x -> case x of
                    CWD txt -> Just txt
                    _       -> Nothing
            txt <- textInput def{_textInputConfig_setValue=setTxt}
            dynClass <- flip mapDyn canSetWD $ \x -> case x of
                True  -> "ui button"
                False -> "ui button disabled"
            (e, _) <- elDynClass' "button" dynClass $ text "Refresh"
            return (_textInput_value txt, domEvent Click e)

        -- Run botton
        (runButton, _) <- elClass' "div" "item" $ dyn $ flip fmap runBtnSt $
            \st -> case st of
                Disabled -> elClass "button" "disabled ui icon labeled button" $ do
                    elClass "i" "play icon" $ return ()
                    text "Run"
                ShowRun -> elClass "button" "ui positive icon labeled button" $ do
                    elClass "i" "play icon" $ return ()
                    text "Run"
                ShowStop -> elClass "button" "ui negative icon labeled button" $ do
                    elClass "i" "pause icon" $ return ()
                    text "Stop"
                ShowLoad -> elClass "button" "ui loading button" $ text "Loading"

        -- Delete button
        (delButton, _) <- elClass' "div" "item" $ do
            dynClass <- flip mapDyn canDelete $ \x -> case x of
                True  -> "ui button negative"
                False -> "ui button negative disabled"
            elDynClass "button" dynClass $ text "Delete"

    return $ MenuEvent (_link_clicked link) runEvts cwdEvts delEvts

data RunButtonState = ShowRun
                    | ShowStop
                    | ShowLoad
                    | Disabled
                    deriving (Eq)

-- | "Run" button handler.
-- Nothing: waiting for result
-- Just _ : result is available
handleMenuRun :: MonadWidget t m
              => ServerResponse t
              -> Dynamic t (S.HashSet T.Text)
              -> Event t ()       -- when clicking the run button
              -> m (Event t Command, Dynamic t RunButtonState)
handleMenuRun (ServerResponse response) clkNode menu_run = do
    st <- foldDynMaybe f Disabled $ leftmost [response', const ShowLoad <$> menu_run]
    let runEvt = tag (current clkNode) $ ffilter (==ShowLoad) $ updated st
    return (Run . S.toList <$> runEvt, st)
  where
    f ShowLoad Disabled = Nothing
    f ShowLoad ShowLoad = Nothing
    f new old           = Just new
    response' = flip fmapMaybe response $ \x -> case x of
        Gr _          -> Just ShowRun
        Status Running -> Just ShowStop
        Status Stopped -> Just ShowRun
        _              -> Nothing

-- | "Set CWD" handler
handleMenuSetWD :: Reflex t
                => Dynamic t RunButtonState
                -> (Event t (), Dynamic t T.Text)
                -> (Event t Command, Dynamic t Bool)
handleMenuSetWD runBtnSt (set_cwd, cwd) = (req, isAvailable)
  where
    isAvailable = flip fmap runBtnSt $ \st -> case st of
        Disabled -> True
        ShowRun  -> True
        ShowLoad -> False
        ShowStop -> False
    req = fmap SetCWD $ tag (current cwd) $ ffilter id $
        tag (current isAvailable) set_cwd

handleDelete :: Reflex t
             => Dynamic t (S.HashSet T.Text)
             -> Dynamic t RunButtonState
             -> Event t ()
             -> (Event t Command, Dynamic t Bool)
handleDelete selection runBtnSt evt = (req, isAvailable)
  where
    req = fmap (Delete . S.toList) $ tag (current selection) $ ffilter id $
        tag (current isAvailable) evt
    isAvailable = zipDynWith (&&) isStop $ fmap (not . S.null) selection
    isStop = flip fmap runBtnSt $ \st -> case st of
        Disabled -> True
        ShowRun  -> True
        ShowLoad -> False
        ShowStop -> False

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
