{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TaijiViz.Client.UI.Home.Menu.Config (configPanel) where

import Control.Monad
import           Reflex.Dom.Core
import Taiji.Types (TaijiConfig(..))
import qualified Data.Text as T
import Data.Aeson
import qualified Data.HashMap.Strict as M
import GHC.Generics

configPanel :: MonadWidget t m => TaijiConfig -> m (Dynamic t TaijiConfig)
configPanel config = elClass "form" "ui form" $ mkConfig config

class Field a where
    toField :: a -> T.Text
    fromField :: T.Text -> a

instance Field String where
    toField = T.pack
    fromField = T.unpack

instance Field a => Field (Maybe a) where
    toField Nothing = ""
    toField (Just x) = toField x
    fromField "" = Nothing
    fromField x = Just $ fromField x

mkConfig :: MonadWidget t m => TaijiConfig -> m (Dynamic t TaijiConfig)
mkConfig TaijiConfig{..} = TaijiConfig
    <$$$> mkField "output_dir" _taiji_output_dir
    <***> mkField "input" _taiji_input
    <***> mkField "picard" _taiji_picard
    <***> mkField "genome" _taiji_genome
    <***> mkField "bwa_index" _taiji_bwa_index
    <***> mkField "star_index" _taiji_star_index
    <***> mkField "annotation" _taiji_annotation
    <***> mkField "rsem_index" _taiji_rsem_index
    <***> mkField "genome_index" _taiji_genome_index
    <***> mkField "motif_file" _taiji_motif_file
  where
    (<$$$>) = fmap . fmap
    (<***>) = (<*>) . fmap (<*>)
    mkField :: (MonadWidget t m, Field a) => T.Text -> a -> m (Dynamic t a)
    mkField name val = divClass "field" $ divClass "ui labeled input" $ do
        divClass "ui label" $ text name
        fmap fromField . _textInput_value <$> textInput
            def{_textInputConfig_initialValue = toField val}
