module Globals where

import qualified SDL
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)
import NanoVG

import Types

-- Colors

editorBackgroundColor, backgroundLines, background, perceptronBodyColor,
    selectionLineColor, selectionFillColor, pinColor,
    perceptronLabelColor :: Color
editorBackgroundColor = rgba  23  34  40 255
backgroundLines       = rgba  72  90 102 255
background            = rgba  47  68  81 255
perceptronBodyColor   = rgba  79 127 141 255
perceptronSelectedBodyColor = rgba 117 187 206 255
selectionLineColor    = rgba 113 180 198 255
selectionFillColor    = rgba 113 180 198 63
pinColor              = rgba 206 201 193 255
knobBaseColor         = rgba  46  73  81 255
knobColor             = pinColor -- rgba 226 221 213 255
perceptronLabelColor  = knobBaseColor

-- Sizes
perceptronWidth, perceptronModuleHeight, perceptronBodyRoundness, pinWidth,
    pinHeight, connectionWidth, knobWidth, knobHeight, knobRoundness :: Float
perceptronWidth         = 100
perceptronModuleHeight  = 40
perceptronBodyRoundness = 10
pinWidth                = 20
pinHeight               = perceptronModuleHeight * 0.6
connectionWidth         = 10
knobWidth               = editorGridSizeF
knobHeight              = editorGridSizeF / 2
knobRoundness           = perceptronBodyRoundness / 2

editorGridSize :: Int
editorGridSize = 40
editorGridSizeF :: Float
editorGridSizeF = 40
connectorSegmentCount :: Float
connectorSegmentCount = 20
tuneMouseDistance :: Float
tuneMouseDistance = 100
perceptronLabelSize :: Float
perceptronLabelSize = 18