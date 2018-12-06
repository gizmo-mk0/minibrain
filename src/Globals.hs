module Globals where

import qualified SDL
import GHC.Word (Word8(..))

import Types

mkColor :: Word8 -> Word8 -> Word8 -> Word8 -> Color
mkColor r g b a = SDL.V4 r g b a

editorBackgroundColor :: Color
editorBackgroundColor = mkColor 47 68 81 255

backgroundLines :: Color
backgroundLines = mkColor 72 90 102 255

background :: Color
background = mkColor 47 68 81 255

nodeBodyColor :: Color
nodeBodyColor = mkColor 79 127 141 255

selectionColor :: Color
selectionColor = mkColor 113 180 198 255

pinColor :: Color
pinColor = mkColor 206 201 193 255