{-# LANGUAGE OverloadedStrings #-}
module TaijiViz.Server.Http
    (httpApp) where

import Control.Arrow (second)
import Network.Wai
import Network.HTTP.Types.Status (status200, status400)
import Control.Exception (bracket_)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Map as M

import TaijiViz.Server.Files

staticRouter :: M.Map B.ByteString Response
staticRouter = M.fromList $ map (second response)
    [ ("/rts.js", rts_js)
    , ("/lib.js", lib_js)
    , ("/out.js", out_js)
    , ("/runmain.js", runmain_js)

    , ("/index.css", index_css)
    , ("/", index_html)
    , ("/logo.svg", logo_svg)
    , ("/favicon.ico", favicon_ico)

    , ("/semantic.js", semantic_js)
    , ("/semantic.css", semantic_css)
    , ("/themes/default/assets/fonts/icons.woff2", icons_woff2)
    , ("/themes/default/assets/fonts/icons.woff", icons_woff)
    , ("/themes/default/assets/fonts/icons.svg", icons_svg)
    ]
  where
    response = responseLBS status200 [] . BL.fromStrict

badRequest :: Response
badRequest = responseLBS status400 [] BL.empty

httpApp :: Application
httpApp req = \respond -> respond $ M.findWithDefault badRequest (rawPathInfo req) staticRouter
