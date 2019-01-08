module Globals where

import qualified SDL
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)

import Types

-- Colors
mkColor :: Word8 -> Word8 -> Word8 -> Word8 -> Color
mkColor r g b a = SDL.V4 r g b a

editorBackgroundColor, backgroundLines, background, perceptronBodyColor,
    selectionColor, pinColor :: Color
editorBackgroundColor = mkColor  47  68  81 255
backgroundLines       = mkColor  72  90 102 255
background            = mkColor  47  68  81 255
perceptronBodyColor   = mkColor  79 127 141 255
selectionColor        = mkColor 113 180 198 255
pinColor              = mkColor 206 201 193 255

-- Sizes
perceptronWidth, perceptronModuleHeight, perceptronBodyRoundness, pinWidth,
    pinHeight, connectionWidth :: CInt
perceptronWidth         = 100
perceptronModuleHeight  =  40
perceptronBodyRoundness =  10
pinWidth                =  20
pinHeight               = round (fromIntegral perceptronModuleHeight * 0.6)
connectionWidth         = 10