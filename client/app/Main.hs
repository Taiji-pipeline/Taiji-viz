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
import           GHCJS.DOM.CanvasRenderingContext2D (fillRect, putImageData,
                                                     setFillStyle)
import           GHCJS.DOM.Element                  (setInnerHTML)
import qualified GHCJS.DOM.Types                    as DOM
import           GHCJS.Marshal                      (toJSVal)
import qualified JavaScript.Web.Canvas              as C
import           Reflex.Dom.Core

import           TaijiViz.Client.Message
import           TaijiViz.Client.Types
import           TaijiViz.Client.UI
import           TaijiViz.Client.Workflow
import           TaijiViz.Common.Types

main :: IO ()
main = mainWidget ui
