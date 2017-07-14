{-# LANGUAGE DeriveGeneric #-}
module TaijiViz.Common.Types where

import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Serialize.Text ()
import Data.Serialize

data Command = Connect T.Text
             | Run [T.Text]
             deriving (Generic)

instance Serialize Command
