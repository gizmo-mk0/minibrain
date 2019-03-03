module Input
    ( InputEvent(..)
    , inputEvent )
    where

import qualified SDL
import qualified Data.Map as Map

import Types

data InputEvent = MouseMoveEvent Vector2f
                | MouseClickEvent Int SDL.MouseButton SDL.InputMotion
                | MouseWheelEvent Int
                | KeyboardEvent SDL.Keycode SDL.InputMotion
                deriving (Eq)

inputEvent :: SDL.Event -> Maybe InputEvent
inputEvent e =
    case SDL.eventPayload e of
        SDL.KeyboardEvent kbe -> Just $
            KeyboardEvent (SDL.keysymKeycode (SDL.keyboardEventKeysym kbe))
                          (SDL.keyboardEventKeyMotion kbe)
        SDL.MouseMotionEvent mme -> Just $
            let (SDL.P mpos) = SDL.mouseMotionEventPos mme
            in  MouseMoveEvent (fmap fromIntegral mpos)
        SDL.MouseButtonEvent mbe ->
            Just $
                MouseClickEvent (fromIntegral $ SDL.mouseButtonEventClicks mbe)
                                (SDL.mouseButtonEventButton mbe)
                                (SDL.mouseButtonEventMotion mbe)
        SDL.MouseWheelEvent mwe -> Just $
            let (SDL.V2 _ amount) = SDL.mouseWheelEventPos mwe
            in  case SDL.mouseWheelEventDirection mwe of
                SDL.ScrollNormal  -> MouseWheelEvent $ fromIntegral amount
                SDL.ScrollFlipped -> MouseWheelEvent $ fromIntegral (-amount)
        _ -> Nothing
