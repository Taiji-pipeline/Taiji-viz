{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module TaijiViz.Server.Types where

import           Data.Binary           (Binary (..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

data RankTable = RankTable
    { rowNames    :: [B.ByteString]
    , colNames    :: [B.ByteString]
    , ranks       :: [[Double]]
    , expressions :: [[Double]]
    } deriving (Eq, Generic)

instance Binary RankTable

data TaijiResults = TaijiResults
    { ranktable :: RankTable
    , nets      :: M.Map T.Text (M.Map B.ByteString B.ByteString)
    } deriving (Generic)

instance Binary TaijiResults

data Network = Network
    { tf :: T.Text
    , parents :: [B.ByteString]
    , children :: [B.ByteString]
    , title :: T.Text
    } deriving (Generic)

instance Binary Network
