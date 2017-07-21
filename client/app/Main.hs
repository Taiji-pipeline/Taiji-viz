{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}

module Main where

#ifndef ghcjs_HOST_OS
import           Control.Monad                      (void)
import           Language.Javascript.JSaddle.Object (js1, jsg1)
#endif

import           Control.Lens
import           Control.Monad                      (forM_)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.ByteString.Char8              as B
import           Data.Maybe                         (fromJust)
import           Data.Serialize                     (decode)
import qualified Data.Text                          as T
import           GHCJS.DOM.CanvasRenderingContext2D (fillRect, putImageData,
                                                     setFillStyle)
import           GHCJS.DOM.Element                  (setInnerHTML)
import qualified GHCJS.DOM.Types                    as DOM
import           GHCJS.Marshal                      (toJSVal)
import qualified JavaScript.Web.Canvas              as C
import           Reflex.Dom.Core
import           Reflex.Dom.Xhr                     (XMLHttpRequest,
                                                     newXMLHttpRequest,
                                                     xhrRequest)

import           TaijiViz.Client.Message
import           TaijiViz.Client.Types
import           TaijiViz.Client.UI
import           TaijiViz.Client.Workflow
import           TaijiViz.Common.Types

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

main :: IO ()
main = mainWidget ui
