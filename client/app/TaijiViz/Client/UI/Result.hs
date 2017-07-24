{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeFamilies          #-}

module TaijiViz.Client.UI.Result where

import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.Default                   (def)
import           Data.Fixed
import qualified Data.Map                       as M
import qualified Data.Matrix.Unboxed            as MU
import           Data.Maybe                     (fromJust)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Unboxed            as U
import           GHCJS.DOM.Element              (setScrollTop, getScrollLeft, setScrollLeft)
import qualified GHCJS.DOM.Types                as DOM
import qualified JavaScript.Web.Canvas          as C
import           Reflex.Dom.Contrib.Widgets.Svg (svg, svgAttr, svgAttr',
                                                 svgDynAttr')
import           Reflex.Dom.Core
import           Reflex.Dom.Widget.Lazy         (virtualList)
import           Statistics.Function            (minMax)
import           Statistics.Sample (meanVarianceUnb)

import           TaijiViz.Client.Message        (httpUrl)
import           TaijiViz.Common.Types

result :: MonadWidget t m => m ()
result = elAttr "div"
    [ ("class", "ui container")
    , ("style", "height:100%")
    ] $ do
        table <- fetchData
        container $ fmapMaybe id table
        return ()

container :: MonadWidget t m => Event t RankTable -> m ()
container table = elAttr "div" style $ do
    widgetHold (text "Loading") (fmap (drawTable 50 (5, 20)) table)
    return ()
  where
    style = toStyleAttr $
        "position" =: "relative" <>
        "width" =: "100%" <>
        "height" =: "calc(100% - 20px)" <>
        "overflow" =: "hidden"

fetchData :: MonadWidget t m => m (Event t (Maybe RankTable))
fetchData = do
    rec (e, _) <- elDynAttr' "i" attr $ return ()
        let clickEvt = domEvent Click e
            req= const url <$> ffilter not (updated canClick)
            attr = flip fmap canClick $ \x -> if x
                then [("class", "refresh icon")]
                else [("class", "refresh icon loading")]
        recv <- fmap fromJust <$> getAndDecode req
        canClick <- holdUniqDyn =<< holdDyn True (leftmost [const False <$> clickEvt, const True <$> recv])
    return recv
  where
    url = httpUrl `T.append` "/data/table"

canvasAttr :: (DOM.MonadJSM m, DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
           => M.Map T.Text T.Text
           -> m (C.Canvas, Element EventResult (DomBuilderSpace m) t)
canvasAttr attr = do
    (e, _) <- elAttr' "canvas" attr $ return ()
    c <- liftIO $ fmap C.unsafeToCanvas $ DOM.toJSVal $ _element_raw e
    return (c, e)

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

drawTable :: MonadWidget t m => Int -> (Double, Double) -> RankTable -> m ()
drawTable unit (minR, maxR) table@RankTable{..} = do
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
                    y = T.pack $ show $ top - 5
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
                [ ("x", T.pack $ show $ left - 5)
                , ("y", T.pack $ show $ (fromIntegral i + 0.5) * fromIntegral unit)
                , ("font-size", "13")
                , ("text-anchor", "end")
                , ("alignment-baseline", "middle")
                ] $ text $ T.pack $ B.unpack txt

    elAttr "div" cornerStyle $ return ()

    liftIO $ do
        C.setWidth w cvs
        C.setHeight h cvs
        drawResults (minR, maxR) (fromIntegral unit) (255,0,0) table cvs
  where
    (r, c) = MU.dim expressions
    top = 80
    left = 80
    w = unit * c + left
    h = unit * r + top
    leftPx = T.pack (show left) <> "px"
    topPx = T.pack (show top) <> "px"
    wrapperStyle = toStyleAttr $ "position" =: "relative" <>
        "width" =: "100%" <> "height" =: "100%" <> "overflow" =: "scroll"
    canvasStyle = toStyleAttr $ "position" =: "relative" <>
        "top" =: topPx <>
        "left" =: leftPx
    topHeaderWrapper = toStyleAttr $ "position" =: "absolute" <>
        "top" =: "0px" <> "left" =: leftPx <>
        "height" =: topPx <>
        "width" =: ("calc(100% - " <> leftPx <> ")") <>
        "overflow" =: "hidden" <>
        "border-radius" =: "8px" <>
        "background-color" =: "rgba(237,237,237,0.9)"
    topHeaderStyle l = toStyleAttr $ "position" =: "absolute" <>
        "top" =: "0px" <> "left" =: (T.pack (show (-l)) <> "px") <>
        "height" =: topPx <>
        "width" =: (T.pack (show w) <> "px")
    leftHeaderWrapper = toStyleAttr $ "position" =: "absolute" <>
        "top" =: topPx <> "left" =: "0px" <>
        "height" =: ("calc(100% - " <> topPx <> ")") <>
        "width" =: leftPx <>
        "overflow" =: "hidden" <>
        "border-radius" =: "8px" <>
        "background-color" =: "rgba(237,237,237,0.9)"
    leftHeaderStyle t = toStyleAttr $ "position" =: "absolute" <>
        "left" =: "0px" <> "top" =: (T.pack (show (-t)) <> "px") <>
        "height" =: (T.pack (show h) <> "px") <>
        "width" =: "100%"
    cornerStyle = toStyleAttr $ "position" =: "absolute" <>
        "width" =: leftPx <> "height" =: topPx <> "top" =: "0px" <>
        "left" =: "0px" <>
        "background-color" =: "white"

drawResults :: (Double, Double)   -- ^ min and max radius
            -> Double             -- ^ The sizes of boxes holding circles
            -> (Double, Double, Double)   -- ^ base color
            -> RankTable
            -> C.Canvas
            -> IO ()
drawResults (minRadius, maxRadius) boxSize color RankTable{..} cvs = do
    ctx <- C.getContext cvs
    flip MU.imapM_ (MU.zip expressions ranks') $ \(i, j) (expr, rank) -> do
        let r = getRadius expr
            c = blend (getWeight rank rangeRank) color white
            x = boxSize * fromIntegral j + boxSize / 2
            y = boxSize * fromIntegral i + boxSize / 2
        drawCircle x y r c ctx
  where
    ranks' = MU.fromRows $ map scale $ MU.toRows ranks
    rangeExpr = minMax $ MU.flatten expressions
    rangeRank = minMax $ MU.flatten ranks'
    getRadius x = minRadius + getWeight x rangeExpr * (maxRadius - minRadius)
    getWeight x (lo, hi) = (x - lo) / (hi - lo)
    white = (255, 255, 255)

toStyleAttr m = "style" =: M.foldWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m

{-
virtualTable :: MonadWidget t m
    => Dynamic t Int -- ^ A 'Dynamic' of the visible region's height in pixels
    -> Int -- ^ The fixed height of each row in pixels
    -> Int -- ^ The index of the row to scroll to on initialization
    -> MU.Matrix Double
    -> m ()
virtualTable heightPx rowPx i0 mat = do
    pb <- getPostBuild
    rec (viewport, result) <- elDynAttr "div" containerStyle $
            elDynAttr' "div" viewportStyle $
            svgAttr "svg" virtualH $ dyn $
                flip fmap window $ \(idx, num) -> forM_ (enumFromTo idx (num+idx-1)) $ \i -> do
                    let row = mat `MU.takeRow` i
                    flip U.imapM_ row $ \j v -> do
                        let r = 5 + (v - min') / (max' - min') * 15
                        svgAttr "circle"
                            [ ("cx", T.pack $ show $ j * 50 + 25)
                            , ("cy", T.pack $ show $ i * 50 + 25)
                            , ("r", T.pack $ show r) ] $ return ()
        scrollPosition <- holdDyn 0 $ leftmost [ round <$> domEvent Scroll viewport
                                               , fmap (const (i0 * rowPx)) pb
                                               ]
        let window = zipDynWith (findWindow rowPx) heightPx scrollPosition
    performEvent_ $ ffor (i0 <$ pb) $ \i ->
      setScrollTop (_element_raw viewport) (i * rowPx)
    uniqWindow <- holdUniqDyn window
    return ()
  where
    virtualH = toStyleAttr $
        "height" =: (T.pack (show $ MU.rows mat * rowPx) <> "px") <>
        "overflow" =: "hidden"
    containerStyle = flip fmap heightPx $ \h -> toStyleAttr $
        "position" =: "relative" <>
        "height" =: (T.pack (show h) <> "px") <>
        "position" =: "relative"
    viewportStyle = flip fmap heightPx $ \h -> toStyleAttr $
        "overflow" =: "auto" <>
        "position" =: "absolute" <>
        "left" =: "0" <>
        "right" =: "0" <>
        "height" =: (T.pack (show h) <> "px")
    findWindow sizeIncrement windowSize startingPosition =
          let startingIndex = startingPosition `div'` sizeIncrement
              numItems = (windowSize + sizeIncrement - 1) `div` sizeIncrement
          in (startingIndex, numItems)
    toStyleAttr m = "style" =: M.foldWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m

    (min', max') = minMax $ MU.flatten mat
    -}

scale :: U.Vector Double -> U.Vector Double
scale xs = U.map (\x -> (x - m) / sqrt s) xs
  where
    (m,s) = meanVarianceUnb xs


-- | Blend two colors
blend :: Double   -- ^ weight
      -> (Double, Double, Double)
      -> (Double, Double, Double)
      -> (Double, Double, Double)
blend w (r1,g1,b1) (r2,g2,b2) =
    ( r1 * w + r2 * (1 - w), g1 * w + g2 * (1 - w), b1 * w + b2 * (1 - w) )
