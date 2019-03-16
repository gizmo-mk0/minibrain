module Types where

import qualified SDL

type Vector2f = SDL.V2 Float
type Vector2i = SDL.V2 Int
data Rect2f = Rect2f
            { topleft :: Vector2f
            , size    :: Vector2f }
            deriving (Eq)

