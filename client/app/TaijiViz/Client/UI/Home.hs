{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module TaijiViz.Client.UI.Home
    ( home
    ) where

import           Control.Arrow                              (second)
import qualified Data.HashSet                               as S
import qualified Data.Text                                  as T
import           Reflex.Dom.Core                            hiding (Delete)
import           Scientific.Workflow.Internal.Builder.Types (_note)

import           TaijiViz.Client.Message
import           TaijiViz.Client.Types
import           TaijiViz.Client.UI.Home.Menu
import           TaijiViz.Client.Workflow                   (NodeEvents (..),
                                                             displayWorkflow)
import           TaijiViz.Common.Types

home :: MonadWidget t m => m (ServerResponse t)
home = do
    pb <- getPostBuild
    rec let reqs = leftmost
                [ const Connect <$> pb
                , _menu_run menuEvts
                , _menu_set_cwd menuEvts
                , _menu_delete menuEvts
                ]
        initialization <- ServerResponse <$> sendMsg reqs
        menuEvts <- header initialization (socketTester initialization)
    return initialization

header :: MonadWidget t m
       => ServerResponse t
       -> m (Dynamic t (S.HashSet T.Text))
       -> m (MenuEvent t)
header response content = do
    divClass "main" $ do
        rec menuEvts <- menu response nodeEvts
            nodeEvts <- content
        return menuEvts

socketTester :: MonadWidget t m
             => ServerResponse t
             -> m (Dynamic t (S.HashSet T.Text))
socketTester (ServerResponse response) = do
    divClass "ui grid" $ do
        (selectedNodes, evtOfevt) <- divClass "twelve wide column" $ do
            rec nodeEvts <- dyn =<< ( holdDyn (return $ NodeEvents never never) $
                    displayWorkflow response selection <$>
                    fmapMaybe getResult response )
                selection <- handleNodeClickEvent nodeEvts
            return (selection, nodeEvts)
        divClass "four wide column" $
            switchPromptly never (_node_hover <$> evtOfevt) >>=
                holdDyn ("", "") . fmap (second _note) >>= displayNodeInfo
        return selectedNodes
  where
    getResult (Gr g) = Just g
    getResult _      = Nothing
    getError (Exception x) = Just x
    getError _             = Nothing

displayNodeInfo :: MonadWidget t m => Dynamic t (T.Text, T.Text) -> m ()
displayNodeInfo info = divClass "info-bar" $
    divClass "ui card" $ divClass "content" $ do
        divClass "header" $ dynText $ fmap fst info
        divClass "description" $ el "p" $ dynText $ fmap snd info
        return ()
{-# INLINE displayNodeInfo #-}

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
