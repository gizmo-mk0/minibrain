module Input
    ( InputData(..)
    , defaultInputData
    , isButtonDown
    , isButtonUp
    , isButtonJustPressed
    , isButtonJustReleased
    , modifyInput )
    where

import qualified SDL
import qualified Data.Map as Map

type ButtonMap = Map.Map SDL.Keycode ButtonState

data InputData = InputData
    { buttons :: ButtonMap
    , mouse   :: MouseData }
data ButtonState = Down | Up | JustPressed | JustReleased deriving (Eq)
data MouseData = MouseData

defaultInputData :: InputData
defaultInputData = InputData Map.empty MouseData

down :: ButtonState -> ButtonState
down bs = if isDown bs then Down else JustPressed

up :: ButtonState -> ButtonState
up bs = if isUp bs then Up else JustReleased

combine :: ButtonState -> ButtonState -> ButtonState
combine Down bs = down bs
combine Up   bs = up   bs
combine _    _  = error "`combine`: The impossible happened"

isButtonDown :: InputData -> SDL.Keycode -> Bool
isButtonDown inp bt = isDown $ Map.findWithDefault Up bt (buttons inp)

isButtonUp :: InputData -> SDL.Keycode -> Bool
isButtonUp inp bt = isUp $ Map.findWithDefault Up bt (buttons inp)

isButtonJustPressed :: InputData -> SDL.Keycode -> Bool
isButtonJustPressed inp bt = isJustPressed $ Map.findWithDefault Up bt (buttons inp)

isButtonJustReleased :: InputData -> SDL.Keycode -> Bool
isButtonJustReleased inp bt = isJustReleased $ Map.findWithDefault Up bt (buttons inp)

isDown :: ButtonState -> Bool
isDown Down        = True
isDown JustPressed = True
isDown _           = False

isUp :: ButtonState -> Bool
isUp Up           = True
isUp JustReleased = True
isUp _            = False

isJustPressed :: ButtonState -> Bool
isJustPressed JustPressed = True
isJustPressed _           = False

isJustReleased :: ButtonState -> Bool
isJustReleased JustReleased = True
isJustReleased _            = False

modifyInput :: InputData -> SDL.Event -> InputData
modifyInput inpDat e =
    let newButtons =
            case SDL.eventPayload e of
                SDL.KeyboardEvent kbe ->
                    let motion = SDL.keyboardEventKeyMotion kbe -- pressed or released
                        keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym kbe)
                    in  modifyButton keycode (fromMotion motion)
                _ -> id
        newMouse = id
    in InputData (newButtons (buttons inpDat)) (newMouse (mouse inpDat))

fromMotion :: SDL.InputMotion -> ButtonState
fromMotion SDL.Pressed  = Down
fromMotion SDL.Released = Up

modifyButton :: SDL.Keycode -> ButtonState -> ButtonMap -> ButtonMap
modifyButton bt action = Map.insertWith combine bt action
