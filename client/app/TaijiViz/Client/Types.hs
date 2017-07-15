{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module TaijiViz.Client.Types where

import           Control.Monad         (mzero)
import           Data.Aeson            (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import           Data.Vector           (Vector)
import           GHC.Generics          (Generic)
import           Reflex.Dom.Core       (Event)

import TaijiViz.Common.Types

type MenuInput t = Event t Result

type MenuEvent t = Vector (Event t ())

instance FromJSON B.ByteString where
    parseJSON (String x) = return $ B.pack $ T.unpack x
    parseJSON _          = mzero

instance ToJSON B.ByteString where
    toJSON x = String $ T.pack $ B.unpack x

data RankTable = RankTable
    { rowNames    :: [B.ByteString]
    , colNames    :: [B.ByteString]
    , ranks       :: [[Double]]
    , expressions :: [[Double]]
    } deriving (Eq, Generic)

instance ToJSON RankTable
instance FromJSON RankTable