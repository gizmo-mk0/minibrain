module Types where

import qualified SDL
import Control.Applicative
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)

type Vector2f = SDL.V2 Float
type Vector2i = SDL.V2 Int