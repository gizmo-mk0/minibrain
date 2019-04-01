module Globals where

import qualified SDL
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)
import Codec.Picture

import Types

-- Colors
mkColor :: Int -> Int -> Int -> Int -> PixelRGBA8
mkColor r g b a = PixelRGBA8 (fromIntegral r) (fromIntegral g)
                             (fromIntegral b) (fromIntegral a)

editorBackgroundColor, backgroundLines, background, perceptronBodyColor,
    selectionLineColor, selectionFillColor, pinColor :: PixelRGBA8
editorBackgroundColor = mkColor  47  68  81 255
backgroundLines       = mkColor  72  90 102 255
background            = mkColor  47  68  81 255
perceptronBodyColor   = mkColor  79 127 141 255
perceptronSelectedBodyColor = mkColor 117 187 206 255
selectionLineColor    = mkColor 113 180 198 255
selectionFillColor    = mkColor 113 180 198 63
pinColor              = mkColor 206 201 193 255
knobBaseColor         = mkColor  46  73  81 255
knobColor             = pinColor -- mkColor 226 221 213 255

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