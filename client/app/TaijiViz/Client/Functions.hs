{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
module TaijiViz.Client.Functions where

import qualified Data.Matrix.Unboxed            as MU
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector as V
import           Statistics.Sample (meanVarianceUnb)
import           Statistics.Function            (minMax)
import           Reflex.Dom.Core

feedbackLoop :: MonadWidget t m
             => Event t a -> (Dynamic t Bool -> m (Event t b)) -> m (Dynamic t Bool)
feedbackLoop feedback fn = do
    rec evt <- fn canClick
        canClick <- holdUniqDyn =<< holdDyn True (leftmost [False <$ evt, True <$ feedback])
    return canClick
