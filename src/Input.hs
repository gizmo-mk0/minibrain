module Input
    ( InputData(..)
    , ButtonType(..)
    , defaultInputData
    , isButtonDown
    , isButtonUp
    , isButtonJustPressed
    , isButtonJustReleased
    , modifyInput )
    where

import qualified SDL
import qualified Data.Map as Map

type ButtonMap = Map.Map ButtonType ButtonState

data InputData = InputData
    { buttons :: ButtonMap
    , mouse   :: MouseData }
data ButtonType = ButtonEsc deriving (Eq, Ord) -- | the rest...
data ButtonState = Down | Up | JustPressed | JustReleased deriving (Eq)
data MouseData = MouseData

defaultInputData :: InputData
defaultInputData = InputData Map.empty MouseData

down :: ButtonState -> ButtonState
down bs = if isDown bs then Down else JustPressed

up :: ButtonState -> ButtonState
up bs = if isUp bs then Up else JustReleased

combine :: ButtonState -> ButtonState -> ButtonState
combine bs Down = down bs
combine bs Up   = up   bs
combine _  _    = Up

isButtonDown :: InputData -> ButtonType -> Bool
isButtonDown inp bt = isDown $ Map.findWithDefault Up bt (buttons inp)

isButtonUp :: InputData -> ButtonType -> Bool
isButtonUp inp bt = isUp $ Map.findWithDefault Up bt (buttons inp)

isButtonJustPressed :: InputData -> ButtonType -> Bool
isButtonJustPressed inp bt = isJustPressed $ Map.findWithDefault Up bt (buttons inp)

isButtonJustReleased :: InputData -> ButtonType -> Bool
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
                    in  modifyButton (sdlInputTranslate keycode)
                                    (fromMotion motion)
                _ -> id
        newMouse = id
    in InputData (newButtons (buttons inpDat)) (newMouse (mouse inpDat))

fromMotion :: SDL.InputMotion -> ButtonState
fromMotion SDL.Pressed  = Down
sromMotion SDL.Released = Up

modifyButton :: Maybe ButtonType -> ButtonState -> ButtonMap -> ButtonMap
modifyButton Nothing   _      = id
modifyButton (Just bt) action = Map.insertWith combine bt action

sdlInputTranslate :: SDL.Keycode -> Maybe ButtonType
sdlInputTranslate SDL.KeycodeEscape = Just ButtonEsc
sdlInputTranslate _                 = Nothing