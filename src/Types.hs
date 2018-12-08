module Types where

import qualified SDL
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)

type Color = SDL.V4 Word8
type Vector2 = SDL.V2 CInt

