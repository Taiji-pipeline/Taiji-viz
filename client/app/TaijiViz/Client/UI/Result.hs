{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeFamilies          #-}

module TaijiViz.Client.UI.Result (result) where

import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Vector.Unboxed              as U
import           Reflex.Dom.Core
import qualified Data.Matrix.Unboxed            as MU

import           Taiji.Types
import           TaijiViz.Client.Functions
import           TaijiViz.Client.Message          (httpUrl)
import           TaijiViz.Client.UI.Result.Config
import           TaijiViz.Client.UI.Result.Table

result :: MonadWidget t m => m ()
result = do
    rec (table, update, toggleConfig) <- menu $ updated feedback
        dynTable <- holdDyn (Right Nothing) table
        feedback <- elAttr "div"
            [ ("class", "ui container")
            , ("style", "height:100%")
            ] $ container $ attach (current config) $
                leftmost [table, tag (current dynTable) update]
        config <- configuration toggleConfig
    return ()

menu :: MonadWidget t m
     => Event t ()
     -> m ( Event t (Either T.Text (Maybe RankTable))
          , Event t ()
          , Dynamic t Bool
          )
menu feedback = divClass "ui vertical inverted borderless icon menu fixed" $ do
    dat <- elClass "a" "item" $ fetchData
    update <- refresh feedback
    rec (options, _) <- elDynAttr' "a" (optBtnSty <$> toggleConfig) $
            elClass "i" "icon options" $ return ()
        toggleConfig <- toggle False $ domEvent Click options
    return (dat, update, toggleConfig)
  where
    optBtnSty x = [("class", "item" <> if x then " active" else "")]

container :: MonadWidget t m
          => Event t (VizConfig, Either T.Text (Maybe RankTable))
          -> m (Dynamic t ())
container dat = elAttr "div" style $ widgetHold initial $ ffor dat $
    \(filtOpt, table) -> case table of
        Left msg -> divClass "ui active inverted dimmer" $
            divClass "ui text massive loader" $ text msg
        Right Nothing -> text "Data is not ready"
        Right (Just t) -> drawTable (5, 20) $ filtering filtOpt t
  where
    filtering (VizConfig filtOpt min') table = case filtOpt of
        Cutoff CV x ->
            let table' = filterRankTable ((>=x). cv) fstFiltResult
            in table' { ranks = MU.fromRows $ map scale $ MU.toRows $ ranks table' }
        Cutoff FoldChange x ->
            let table' = fstFiltResult { ranks = MU.fromRows $ map logFoldChange $ MU.toRows $ ranks fstFiltResult }
            in filterRankTable ((>=x). U.maximum) table'
      where
        fstFiltResult = filterRankTable ((>=min'). U.maximum) table
    style = [ ("style", "position:relative;" <> "width:100%;" <>
        "height:calc(100% - 20px);" <> "overflow:hidden;") ]
    initial = do
        text "Please click "
        elClass "i" "icon download" $ return ()
        text " to retrieve the data"

refresh :: MonadWidget t m => Event t () -> m (Event t ())
refresh feedback = do
    rec (e, _) <- elClass "a" "item" $ elDynAttr' "i" attr $ return ()
        let clickEvt = domEvent Click e
            attr = flip fmap canClick $ \x -> if x
                then [("class", "refresh icon")]
                else [("class", "refresh icon loading")]
        canClick <- holdUniqDyn =<< holdDyn True (leftmost [False <$ clickEvt, True <$ feedback])
    return $ () <$ ffilter not (updated canClick)

-- | Icon and events for retrieving data.
fetchData :: MonadWidget t m
          => m (Event t (Either T.Text (Maybe RankTable)))
fetchData = do
    rec isIdle <- feedbackLoop feedback $ \_ -> do
            (e, _) <- elClass' "i" "download icon" $ return ()
            return $ domEvent Click e
        let req = url <$ ffilter not (updated isIdle)
        feedback <- fmap fromJust <$> getAndDecode req
    return $ leftmost [Left "Downloading" <$ req, Right <$> feedback]
  where
    url = httpUrl `T.append` "/data/table"
