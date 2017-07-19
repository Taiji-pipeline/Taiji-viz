{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Server.Http
    (app) where

import Network.Wai
import Network.HTTP.Types.Status (status200, status400)
import Control.Exception (bracket_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Map as M

import TaijiViz.Server.Files

router :: M.Map B.ByteString Response
router = M.fromList
    [ ("/rts.js", response rts_js)
    , ("/lib.js", response lib_js)
    , ("/out.js", response out_js)
    , ("/runmain.js", response runmain_js)
    , ("/semantic.js", response semantic_js)
    , ("/semantic.css", response semantic_css)
    , ("/index.css", response index_css)
    , ("/", response index_html)
    , ("/themes/default/assets/fonts/icons.woff2", response icons_woff2)
    , ("/themes/default/assets/fonts/icons.woff", response icons_woff)
    , ("/themes/default/assets/fonts/icons.svg", response icons_svg)
    ]
  where
    response = responseLBS status200 [] . BL.fromStrict

badRequest :: Response
badRequest = responseLBS status400 [] BL.empty

app :: Application
app req respond = bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ M.findWithDefault badRequest (rawPathInfo req) router)
