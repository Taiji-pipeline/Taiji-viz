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
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Vector.Unboxed as U
import           Reflex.Dom.Core
import Statistics.Function (minMax)

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

drawTable :: MU.Matrix Double -> C.Canvas -> IO ()
drawTable dat c = do
    C.setWidth (50*w) c
    C.setHeight (50*h) c
    ctx <- C.getContext c
    flip MU.imapM_ dat $ \(i, j) d -> do
        let r = 5 + (d - min') / (max' - min') * 15
            x = 50 * fromIntegral j + 25
            y = 50 * fromIntegral i + 25
        drawCircle x y r ctx
  where
    (min', max') = minMax $ MU.flatten dat
    (h, w) = MU.dim dat
