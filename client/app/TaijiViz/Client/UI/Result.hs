{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists #-}

module TaijiViz.Client.UI.Result where

import           Reflex.Dom.Core
import Data.Default (def)
import Data.Maybe (fromJust)
import qualified Data.Text as T

import TaijiViz.Client.Message (httpUrl)
import TaijiViz.Common.Types

result :: MonadWidget t m => m ()
result = el "div" $ do
    evt <- refresh
    r <- fmap (f . fromJust) <$> getAndDecode (const (httpUrl `T.append` "/data/table") <$> evt)
    holdDyn "0" r >>= dynText
    return ()
  where
    f :: Maybe RankTable -> T.Text
    f Nothing = "Nothing"
    f (Just _) = "Just"

refresh :: MonadWidget t m => m (Event t ())
refresh = do
    (e, _) <- elAttr' "i" [("class", "refresh icon")] $ return ()
    return $ domEvent Click e

canvas :: MonadWidget t m => m C.Canvas
canvas = do
    (e, _) <- elClass' "canvas" "canvas" $ return ()
    liftIO $ fmap C.unsafeToCanvas $ toJSVal $ _element_raw e
  where
    w = 300
    h = 150

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

--baseUrl = "http://yed.ucsd.edu:8889"

dataViewer :: MonadWidget t m => m (Event t RankTable)
dataViewer = do
    ctx <- canvas
    pb <- getPostBuild
    evt <- fmap (fromJust . decodeXhrResponse) <$> performRequestAsync (fmap (const req) pb)
    performEvent_ (fmap (action ctx) evt)
    return evt
  where
    req = xhrRequest "GET" "http://yed.ucsd.edu:8889/data/table" def
    action ctx RankTable{..} = DOM.liftJSM $ do
        drawTable expressions ctx
