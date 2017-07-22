module TaijiViz.Client.Types where

import           Reflex.Dom.Core       (Event)

import TaijiViz.Common.Types

newtype ServerResponse t = ServerResponse (Event t Result)
