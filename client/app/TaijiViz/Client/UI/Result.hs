{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeFamilies          #-}

module TaijiViz.Client.UI.Result
    (result) where

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
import qualified Data.Vector.Unboxed            as U
import           GHCJS.DOM.Element              (setScrollTop, getScrollLeft, setScrollLeft)
import qualified GHCJS.DOM.Types                as DOM
import qualified JavaScript.Web.Canvas          as C
import           Reflex.Dom.Contrib.Widgets.Svg (svg, svgAttr, svgAttr',
                                                 svgDynAttr')
import           Reflex.Dom.Core
import           Statistics.Function            (minMax)

import           TaijiViz.Client.Message        (httpUrl)
import           TaijiViz.Common.Types
import           TaijiViz.Client.UI.Result.Config (configuration)
import           TaijiViz.Client.Functions

result :: MonadWidget t m => m ()
result = do
    rec (table, update, toggleConfig) <- menu $ updated feedback
        dynTable <- holdDyn (Right Nothing) table
        feedback <- elAttr "div"
            [ ("class", "ui container")
            , ("style", "height:100%")
            ] $ container $ attach (current config) $
                leftmost [table, tag (current dynTable) update]
        config <- configuration toggleConfig
    return ()

menu :: MonadWidget t m
     => Event t ()
     -> m ( Event t (Either T.Text (Maybe RankTable))
          , Event t ()
          , Dynamic t Bool
          )
menu feedback = divClass "ui vertical inverted borderless icon menu fixed" $ do
    dat <- elClass "a" "item" $ fetchData
    update <- refresh feedback
    rec (options, _) <- elDynAttr' "a" (optBtnSty <$> toggleConfig) $
            elClass "i" "icon options" $ return ()
        toggleConfig <- toggle False $ domEvent Click options
    return (dat, update, toggleConfig)
  where
    optBtnSty x = [("class", "item" <> if x then " active" else "")]

container :: MonadWidget t m
          => Event t ((Double, Double), Either T.Text (Maybe RankTable))
          -> m (Dynamic t ())
container dat = elAttr "div" style $ do
    widgetHold initial $ ffor dat $ \((th,min'), table) -> case table of
        Left msg -> divClass "ui active inverted dimmer" $
            divClass "ui text massive loader" $ text msg
        Right Nothing -> text "Data is not ready"
        Right (Just t) ->
            drawTable 50 (5, 20) $ filterRankTable ((>=th). cv) $
                filterRankTable ((>=min'). U.maximum) t
  where
    style = toStyleAttr $
        "position" =: "relative" <>
        "width" =: "100%" <>
        "height" =: "calc(100% - 20px)" <>
        "overflow" =: "hidden"
    initial = do
        text "Please click "
        elClass "i" "icon download" $ return ()
        text " to retrieve the data"

refresh :: MonadWidget t m => Event t () -> m (Event t ())
refresh feedback = do
    rec (e, _) <- elClass "a" "item" $ elDynAttr' "i" attr $ return ()
        let clickEvt = domEvent Click e
            attr = flip fmap canClick $ \x -> if x
                then [("class", "refresh icon")]
                else [("class", "refresh icon loading")]
        canClick <- holdUniqDyn =<< holdDyn True (leftmost [False <$ clickEvt, True <$ feedback])
    return $ () <$ ffilter not (updated canClick)

-- | Icon and events for retrieving data.
fetchData :: MonadWidget t m
          => m (Event t (Either T.Text (Maybe RankTable)))
fetchData = do
    rec isIdle <- feedbackLoop feedback $ \_ -> do
            (e, _) <- elClass' "i" "download icon" $ return ()
            return $ domEvent Click e
        let req = url <$ ffilter not (updated isIdle)
        feedback <- fmap fromJust <$> getAndDecode req
    return $ leftmost [Left "Downloading" <$ req, Right <$> feedback]
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
