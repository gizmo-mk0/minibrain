module Globals where

import qualified Graphics.Gloss.Rendering as G
import qualified SDL
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)

import Types

-- Colors
mkColor :: Int -> Int -> Int -> Int -> G.Color
mkColor r g b a = G.makeColorI r g b a

editorBackgroundColor, backgroundLines, background, perceptronBodyColor,
    selectionLineColor, selectionFillColor, pinColor :: G.Color
editorBackgroundColor = mkColor  47  68  81 255
backgroundLines       = mkColor  72  90 102 255
background            = mkColor  47  68  81 255
perceptronBodyColor   = mkColor  79 127 141 255
selectionLineColor    = mkColor 113 180 198 255
selectionFillColor    = mkColor 113 180 198 63
pinColor              = mkColor 206 201 193 255

-- Sizes
perceptronWidth, perceptronModuleHeight, perceptronBodyRoundness, pinWidth,
    pinHeight, connectionWidth :: Float
perceptronWidth         = 100
perceptronModuleHeight  = 40
perceptronBodyRoundness = 10
pinWidth                = 20
pinHeight               = perceptronModuleHeight * 0.6
connectionWidth         = 10