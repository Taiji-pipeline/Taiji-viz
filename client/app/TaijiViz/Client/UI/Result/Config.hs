{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TaijiViz.Client.UI.Result.Config
    ( configuration
    , VizConfig(..)
    , Cutoff(..)
    , CutoffType(..)
    ) where

import           Reflex.Dom.Core
import Control.Monad
import qualified Data.Text as T
import Data.Text.Read (double)
import           Data.Monoid                    ((<>))
import qualified GHCJS.DOM.Element as DOM
import GHCJS.DOM.Types (liftJSM, JSVal, pFromJSVal, pToJSVal, toJSVal, JSM)

data VizConfig = VizConfig
    { _vizconfig_cutoff :: Cutoff
    , _vizconfig_min_rank :: Double
    }

data Cutoff = Cutoff CutoffType Double

data CutoffType = FoldChange
                | CV
                deriving (Eq)

configuration :: MonadWidget t m => Dynamic t Bool -> m (Dynamic t VizConfig)
configuration shouldShow = elDynAttr "div" (wrapperStyle <$> shouldShow) $ do
    elAttr "div"
        [ ("class", "ui vertical pointing secondary menu")
        , ("style", "width:100px;float:left") ] $ do
            elClass "a" "item active" $ text "filter"
            elClass "a" "item" $ text "sort"

    elClass "form" "ui form" $ divClass "fields" $ do
        cv <- do
            cutoffType <- cutoffConfig
            val <- divClass "field" $ do
                divClass "ui labeled input" $ do
                    divClass "ui label" $ text "Cutoff:"
                    txt <- doubleInput def{_textInputConfig_initialValue="1"}
                    holdDyn 1 $ fmapMaybe fromEither txt
            return $ zipDynWith Cutoff cutoffType val
        m <- divClass "field" $ do
            divClass "ui labeled input" $ do
                divClass "ui label" $ text "min. rank:"
                txt <- doubleInput def{_textInputConfig_initialValue="1e-4"}
                holdDyn 1e-4 $ fmapMaybe fromEither txt
        return $ zipDynWith VizConfig cv m
  where
    wrapperStyle display =
        [ ( "style", viz <> "background-color:#F8F8F8;" <> "width:100%;"
        <> "position:fixed;" <> "border-radius:10px;" <> "padding:10px;" ) ]
      where
        viz = if display
            then "bottom:10px;transition:bottom 0.5s linear;"
            else "bottom:-300px;transition:bottom 0.5s linear;"

cutoffConfig :: MonadWidget t m => m (Dynamic t CutoffType)
cutoffConfig = radioGroup 0
    [ (FoldChange, "fold change")
    , (CV, "coefficient of variance") ]

doubleInput :: MonadWidget t m => TextInputConfig t -> m (Event t (Either String (Double, T.Text)))
doubleInput opt = do
    txt <- textInput opt{_textInputConfig_inputType = "number"}
    return $ fmap double $ tag (current (_textInput_value txt)) $ ffilter not $
        updated $ _textInput_hasFocus txt

fromEither (Left _) = Nothing
fromEither (Right (x,y)) = if T.null y then Just x else Nothing

radioGroup :: MonadWidget t m => Int -> [(a, T.Text)] -> m (Dynamic t a)
radioGroup i vals = do
    es <- forM (zip [0..] vals) $ \(i', (_, label)) ->
        putRadioItem "CutoffType" label (i'==i)
    let evts = leftmost $ map (domEvent Change) es
    r <- performEvent $ liftJSM . (\_ -> (fst . (vals!!)) <$> getIdx (map _element_raw es)) <$> evts
    holdDyn (fst $ vals !! i) r
  where
    getIdx es = do
        checked <- mapM (\x -> toJSVal x >>= js_isChecked) es
        let [(idx, _)] = filter snd $ zip [0..] checked
        return idx

-- | Make an individual radio checkbox item.
putRadioItem :: MonadWidget t m
             => T.Text
             -> T.Text
             -> Bool
             -> m _
putRadioItem name label checked = do
    (inputEl, _) <- elAttr' "input" attrs blank
    el "label" $ text label
    return inputEl
  where
    attrs = [("type", "radio"), ("name", name)] <>
        (if checked then [("checked", "true")] else [])
foreign import javascript unsafe "$1.checked"
  js_isChecked :: JSVal -> JSM Bool
