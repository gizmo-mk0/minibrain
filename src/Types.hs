module Types where

import qualified SDL
import qualified NanoVG as NVG

type Vector2f = SDL.V2 Float
type Vector2i = SDL.V2 Int
data Rect2f = Rect2f
            { topleft :: Vector2f
            , size    :: Vector2f }
            deriving (Eq)

data Config = Config
            { getWindow     :: SDL.Window
            , getWindowSize :: Vector2i
            , getNvgContext :: NVG.Context }

-- data CameraData = CameraData
--                 { cPosition   :: Vector2f
--                 , cRotation   :: Float
--                 , cZoom       :: Float }
--                 deriving (Show)