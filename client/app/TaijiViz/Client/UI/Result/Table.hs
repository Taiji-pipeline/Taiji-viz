{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module TaijiViz.Client.UI.Result.Table (drawTable) where

import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Map                       as M
import qualified Data.Matrix.Unboxed            as MU
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           GHCJS.DOM.Element              (getScrollLeft)
import qualified GHCJS.DOM.Types                as DOM
import qualified JavaScript.Web.Canvas          as C
import           Reflex.Dom.Contrib.Widgets.Svg (svgAttr, svgDynAttr')
import           Reflex.Dom.Core
import           Statistics.Function            (minMax)

import           Taiji.Types
import           TaijiViz.Client.Functions (blend)

drawTable :: MonadWidget t m => (Double, Double) -> RankTable -> m ()
drawTable (minR, maxR) table@RankTable{..} = do
    (wrapper, (cvs, _)) <- elAttr' "div" wrapperStyle $ canvasAttr canvasStyle
    scrollTopPosition <- holdDyn 0 $ domEvent Scroll wrapper
    let scrollLeft = liftIO $ fmap fromIntegral $ getScrollLeft $
            DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw wrapper
    scrollLeftPosition <- holdDyn (0::Double) =<<
        performEvent (scrollLeft <$ domEvent Scroll wrapper)

    elAttr "div" topHeaderWrapper $
        svgDynAttr' "svg" (topHeaderStyle <$> scrollLeftPosition) $
            flip V.imapM_ colNames $ \i txt -> do
                let x = T.pack $ show $ (fromIntegral i + 0.5) * fromIntegral unit
                    y = T.pack $ show $ top - 8
                svgAttr "text"
                    [ ("x", x)
                    , ("y", y)
                    , ("font-size", "14")
                    , ("alignment-baseline", "middle")
                    , ("transform", "rotate(-40 " <> x <> "," <> y <> ")")
                    ] $ text txt

    elAttr "div" leftHeaderWrapper $
        svgDynAttr' "svg" (leftHeaderStyle <$> scrollTopPosition) $
            flip V.imapM_ rowNames $ \i txt -> svgAttr "text"
                [ ("x", T.pack $ show $ left - 8)
                , ("y", T.pack $ show $ (fromIntegral i + 0.5) * fromIntegral unit)
                , ("font-size", "13")
                , ("text-anchor", "end")
                , ("alignment-baseline", "middle")
                ] $ text txt

    elAttr "div" cornerStyle $ return ()

    liftIO $ do
        C.setWidth w cvs
        C.setHeight h cvs
        drawResults (minR, maxR) (fromIntegral unit) (255,0,0) table cvs
  where
    unit = truncate $ maxR * 2.5
    (r, c) = MU.dim ranks
    top = 80
    left = 80
    w = unit * c + left
    h = unit * r + top
    leftPx = T.pack (show left) <> "px"
    topPx = T.pack (show top) <> "px"
    wrapperStyle = M.singleton "style" $ "position:relative;" <>
        "width:100%;" <> "height:100%;" <> "overflow:scroll;"
    canvasStyle = M.singleton "style" $ "position:relative;" <>
        "top:" <> T.pack (show top) <> "px;" <>
        "left:" <> T.pack (show left) <> "px;"
    topHeaderWrapper = M.singleton "style" $ "position:absolute;" <>
        "top:0px;" <> "left:" <> T.pack (show left) <> "px;" <>
        "height:" <> T.pack (show top) <> "px;" <>
        "width:" <> "calc(100% - " <> T.pack (show left) <> "px);" <>
        "overflow:hidden;" <> "border-radius:8px;" <>
        "background-color:rgba(237,237,237,0.9);"
    topHeaderStyle l = M.singleton "style" $ "position:absolute;" <>
        "top:0px;" <> "left:" <> T.pack (show (-l)) <> "px;" <>
        "height:" <> T.pack (show top) <> "px;" <>
        "width:" <> T.pack (show w) <> "px;"
    leftHeaderWrapper = M.singleton "style" $ "position:absolute;" <>
        "top:" <> T.pack (show top) <> "px;" <> "left:0px;" <>
        "height:" <> "calc(100% - " <> T.pack (show top) <> "px);" <>
        "width:" <> T.pack (show left) <> "px;" <>
        "overflow:hidden;" <> "border-radius:8px;" <>
        "background-color:rgba(237,237,237,0.9);"
    leftHeaderStyle t = M.singleton "style" $ "position:absolute;" <>
        "left:0px;" <> "top:" <> T.pack (show (-t)) <> "px;" <>
        "height:" <> T.pack (show h) <> "px;" <> "width:100%;"
    cornerStyle = M.singleton "style" $ "position:absolute;" <>
        "width:" <> T.pack (show left) <> "px;" <>
        "height:" <> T.pack (show top) <> "px;" <> "top:0px;" <>
        "left:0px;" <> "background-color:white;"

drawResults :: (Double, Double)   -- ^ min and max radius
            -> Double             -- ^ The sizes of boxes holding circles
            -> (Double, Double, Double)   -- ^ base color
            -> RankTable
            -> C.Canvas
            -> IO ()
drawResults (minRadius, maxRadius) boxSize color RankTable{..} cvs = do
    ctx <- C.getContext cvs
    flip MU.imapM_ ranks $ \(i, j) rank -> do
        let r = fromMaybe maxRadius $ fmap (MU.! (i,j)) expressions >>= getRadius
            c = blend (0.02 + getWeight rank rangeRank) color white
            x = boxSize * fromIntegral j + boxSize / 2
            y = boxSize * fromIntegral i + boxSize / 2
        drawCircle x y r c ctx
  where
    rangeExpr = fmap (minMax . MU.flatten) expressions
    rangeRank = minMax $ MU.flatten ranks
    getRadius x = do
        r <- rangeExpr
        return $ minRadius + getWeight x r * (maxRadius - minRadius)
    getWeight x (lo, hi) = (x - lo) / (hi - lo)
    white = (255, 255, 255)

drawCircle :: Double   -- ^ X coordinate
           -> Double   -- ^ Y coordinate
           -> Double   -- ^ Radius
           -> (Double, Double, Double)  -- ^ color
           -> C.Context -> IO ()
drawCircle x y radius (r,g,b) ctx = do
    C.beginPath ctx
    C.fillStyle (round r) (round g) (round b) 1 ctx
    C.arc x y radius 0 (2*pi) False ctx
    C.fill ctx
    C.closePath ctx

-- | Create HTML canvas element.
canvasAttr :: (DOM.MonadJSM m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
           => M.Map T.Text T.Text
           -> m (C.Canvas, Element EventResult (DomBuilderSpace m) t)
canvasAttr attr = do
    (e, _) <- elAttr' "canvas" attr $ return ()
    c <- liftIO $ fmap C.unsafeToCanvas $ DOM.toJSVal $ _element_raw e
    return (c, e)
