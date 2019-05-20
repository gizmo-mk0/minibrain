module Types where

import qualified SDL

type Vector2f = SDL.V2 Float
type Vector2i = SDL.V2 Int
data Rect2f = Rect2f
            { topleft :: Vector2f
            , size    :: Vector2f }
            deriving (Eq)

data InputEvent = MouseMoveEvent Vector2f
                | MouseClickEvent Int SDL.MouseButton SDL.InputMotion
                | MouseWheelEvent Int
                | KeyboardEvent SDL.Keycode SDL.InputMotion
                deriving (Eq)