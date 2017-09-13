{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo           #-}
module TaijiViz.Client.UI.MessageBoard where

import           Reflex.Dom.Core
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List

import TaijiViz.Client.Types
import           TaijiViz.Common.Types

messageBoard :: MonadWidget t m => ServerResponse t -> m ()
messageBoard (ServerResponse response) = do
    rec msg <- (fmap . fmap) snd $ foldDyn f (0, M.empty) $
            leftmost [fmap Right closeEvt, fmapMaybe getError response]
        closeEvt <- showMessage msg
    return ()
  where
    getError (Exception x) = Just $ Left x
    getError _             = Nothing
    f (Left x) (!i, !acc) = (i+1, M.insert (i+1) x $ acc)
    f (Right x) (!i, !acc) = (i, foldl' (\m k -> M.delete k m) acc $ M.keys x)

showMessage :: MonadWidget t m
            => Dynamic t (M.Map Int T.Text)
            -> m (Event t (M.Map Int ()))
showMessage x = listViewWithKey x $ \_ txt -> do
    divClass "ui negative message" $ do
        (closeIcon, _) <- elAttr' "i" [("class", "close icon")] $ return ()
        divClass "header" $ text "There were some errors"
        el "p" $ dynText txt
        return $ domEvent Click closeIcon
