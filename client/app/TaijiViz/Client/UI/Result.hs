{-# LANGUAGE OverloadedStrings     #-}

module TaijiViz.Client.UI.Result where

import           Reflex.Dom.Core

result :: MonadWidget t m => m ()
result = el "div" $ text "Not ready"
