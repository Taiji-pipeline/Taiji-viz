{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module TaijiViz.Client.UI.Result.Config
    (configuration) where

import           Reflex.Dom.Core
import qualified Data.Text as T
import Data.Text.Read (double)
import           Data.Monoid                    ((<>))

configuration :: MonadWidget t m => Dynamic t Bool -> m (Dynamic t (Double, Double))
configuration shouldShow = elDynAttr "div" (wrapperStyle <$> shouldShow) $ do
    elAttr "div"
        [ ("class", "ui vertical pointing secondary menu")
        , ("style", "width:100px;float:left") ] $ do
            elClass "a" "item active" $ text "filter"
            elClass "a" "item" $ text "sort"

    elClass "form" "ui form" $ divClass "fields" $ do
        cv <- divClass "field" $ do
            divClass "ui labeled input" $ do
                divClass "ui label" $ text "Coefficient of variance:"
                txt <- doubleInput def{_textInputConfig_initialValue="1"}
                holdDyn 1 $ fmapMaybe fromEither txt
        m <- divClass "field" $ do
            divClass "ui labeled input" $ do
                divClass "ui label" $ text "min. rank:"
                txt <- doubleInput def{_textInputConfig_initialValue="1e-4"}
                holdDyn 1e-4 $ fmapMaybe fromEither txt
        return $ zipDyn cv m
  where
    wrapperStyle display =
        [( "style", viz <> "background-color:#F8F8F8;" <>
                    "width:100%;" <> "position:fixed;" <>
                    "border-radius:10px;" <> "padding:10px;" )
        ]
      where
        viz = if display
            then "bottom:10px;transition:bottom 0.5s linear;"
            else "bottom:-300px;transition:bottom 0.5s linear;"

doubleInput :: MonadWidget t m => TextInputConfig t -> m (Event t (Either String (Double, T.Text)))
doubleInput opt = do
    txt <- textInput opt{_textInputConfig_inputType = "number"}
    return $ fmap double $ tag (current (_textInput_value txt)) $ ffilter not $
        updated $ _textInput_hasFocus txt

fromEither (Left _) = Nothing
fromEither (Right (x,y)) = if T.null y then Just x else Nothing
