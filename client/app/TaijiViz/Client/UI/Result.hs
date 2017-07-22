{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module TaijiViz.Client.UI.Result where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Default            (def)
import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import qualified GHCJS.DOM.Types         as DOM
import qualified JavaScript.Web.Canvas   as C
import           Reflex.Dom.Core

import           TaijiViz.Client.Message (httpUrl)
import           TaijiViz.Common.Types

result :: MonadWidget t m => m ()
result = el "div" $ do
    table <- fetchData
    (c,_) <- divClass "canvas" $ canvas
    performEvent_ (fmap (action c) table)
    return ()
  where
    action ctx (Just RankTable{..}) = DOM.liftJSM $ do
        drawTable expressions ctx
    action _ _ = return ()

fetchData :: MonadWidget t m => m (Event t (Maybe RankTable))
fetchData = do
    rec (e, _) <- elDynAttr' "i" attr $ return ()
        let clickEvt = domEvent Click e
            req= const url <$> ffilter not (updated canClick)
            attr = flip fmap canClick $ \x -> if x
                then [("class", "refresh icon")]
                else [("class", "refresh icon loading")]
        recv <- fmap fromJust <$> getAndDecode req
        canClick <- holdDyn True $ leftmost [const False <$> clickEvt, const True <$> recv]
    return recv
  where
    url = httpUrl `T.append` "/data/table"

canvas :: (DOM.MonadJSM m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
       => m (C.Canvas, Element EventResult (DomBuilderSpace m) t)
canvas = do
    (e, _) <- el' "canvas" $ return ()
    c <- liftIO $ fmap C.unsafeToCanvas $ DOM.toJSVal $ _element_raw e
    return (c, e)

drawCircle :: Double -> Double -> Double -> C.Context -> IO ()
drawCircle x y r ctx = do
    C.beginPath ctx
    C.arc x y r 0 (2*pi) False ctx
    C.fill ctx
    C.closePath ctx

drawTable :: [[Double]] -> C.Canvas -> IO ()
drawTable dat c = do
    C.setWidth w c
    C.setHeight h c
    ctx <- C.getContext c
    forM_ dat' $ \((x,y),d) -> do
        let r = 5 + (d - min') / (max' - min') * 15
        drawCircle x y r ctx
  where
    dat' = concat $ zipWith f [25, 75 ..] dat
    max' = maximum $ snd $ unzip dat'
    min' = minimum $ snd $ unzip dat'
    f y row = zipWith (\x d -> ((x,y),d)) [25, 70 ..] row
    w = 50 * length (head dat)
    h = 50 * length dat
