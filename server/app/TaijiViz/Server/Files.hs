{-# LANGUAGE TemplateHaskell #-}
module TaijiViz.Server.Files where

import Data.FileEmbed
import Data.ByteString (ByteString)

favicon_ico :: ByteString
favicon_ico = $(embedFile "data/favicon.ico")

logo_svg :: ByteString
logo_svg = $(embedFile "data/logo.svg")

rts_js :: ByteString
rts_js = $(embedFile "data/rts.js")

out_js :: ByteString
out_js = $(embedFile "data/out.js")

lib_js :: ByteString
lib_js = $(embedFile "data/lib.js")

runmain_js :: ByteString
runmain_js = $(embedFile "data/runmain.js")

index_html :: ByteString
index_html = $(embedFile "data/index.html")

index_css :: ByteString
index_css = $(embedFile "data/index.css")

semantic_js :: ByteString
semantic_js = $(embedFile "data/semantic.min.js")

semantic_css :: ByteString
semantic_css = $(embedFile "data/semantic.min.css")

icons_woff :: ByteString
icons_woff = $(embedFile "data/icons.woff")

icons_woff2 :: ByteString
icons_woff2 = $(embedFile "data/icons.woff2")

icons_svg :: ByteString
icons_svg = $(embedFile "data/icons.svg")
