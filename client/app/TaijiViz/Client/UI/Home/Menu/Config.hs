{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Client.UI.Home.Menu.Config where

import Control.Monad
import           Reflex.Dom.Core
import Taiji.Types (TaijiConfig(..))
import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.Default

configPanel :: MonadWidget t m => m ()
configPanel = elClass "form" "ui form" $ do
    case json of
        Object obj -> forM_ (M.toList obj) $ \(key, val) -> do
            let val' = case val of
                    String txt -> txt
                    Null -> ""
                    _ -> ""
            divClass "field" $ do
                divClass "ui labeled input" $ do
                    divClass "ui label" $ text key
                    textInput def{_textInputConfig_initialValue=val'}
        _ -> return ()
  where
    json = toJSON (def :: TaijiConfig)

    {-
field :: TaijiConfig ->
field config =


key val = divClass "field" $ divClass "ui labeled input" $ do
    divClass "ui label" $ text key
    textInput def{_textInputConfig_initialValue=val'}
  where
    val' = case val of
        String txt -> txt
        Null -> ""
        _ -> ""
        -}
