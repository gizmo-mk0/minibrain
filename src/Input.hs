module Input
    ( InputData(..)
    , MouseData(..)
    , defaultInputData
    , isButtonDown
    , isButtonUp
    , isButtonJustPressed
    , isButtonJustReleased
    , isMouseButtonDown
    , isMouseButtonUp
    , isMouseButtonJustPressed
    , isMouseButtonJustReleased
    , modifyInput
    , advanceInputData )
    where

import qualified SDL
import qualified Data.Map as Map

import Types

type ButtonMap a = Map.Map a ButtonState
type KeyboardButtonMap = ButtonMap SDL.Keycode
type MouseButtonMap = ButtonMap SDL.MouseButton

data InputData = InputData
    { buttons :: KeyboardButtonMap
    , mouse   :: MouseData }
    deriving (Show)
data ButtonState = Down | Up | JustPressed | JustReleased deriving (Show, Eq)
data MouseData = MouseData
               { mousePosition :: Vector2i
               , mouseButtons  :: MouseButtonMap }
               deriving (Show)

defaultInputData :: InputData
defaultInputData = InputData Map.empty (MouseData (SDL.V2 0 0) Map.empty)

down :: ButtonState -> ButtonState
down bs = if isDown bs then Down else JustPressed

up :: ButtonState -> ButtonState
up bs = if isUp bs then Up else JustReleased

-- combine action previousState
combine :: ButtonState -> Maybe ButtonState -> Maybe ButtonState
combine Down Nothing   = Just $ down Up
combine Up   Nothing   = Just $ up   Up
combine Down (Just bs) = Just $ down bs
combine Up   (Just bs) = Just $ up   bs
combine _    _         = error "`combine`: The impossible happened"
    -- This happens when `combine` receives something else other than Up
    -- (meaning the button is currently released) or Down (meaning the button is
    -- currently pressed). This means there is an error in the code somewhere.

isButtonDown :: InputData -> SDL.Keycode -> Bool
isButtonDown inp bt = isDown $ Map.findWithDefault Up bt (buttons inp)

isButtonUp :: InputData -> SDL.Keycode -> Bool
isButtonUp inp bt = isUp $ Map.findWithDefault Up bt (buttons inp)

isMouseButtonDown :: InputData -> SDL.MouseButton -> Bool
isMouseButtonDown inp bt =
    isDown $ Map.findWithDefault Up bt (mouseButtons . mouse $ inp)

isMouseButtonUp :: InputData -> SDL.MouseButton -> Bool
isMouseButtonUp inp bt =
    isUp $ Map.findWithDefault Up bt (mouseButtons . mouse $ inp)

isButtonJustPressed :: InputData -> SDL.Keycode -> Bool
isButtonJustPressed inp bt =
    isJustPressed $ Map.findWithDefault Up bt (buttons inp)

isButtonJustReleased :: InputData -> SDL.Keycode -> Bool
isButtonJustReleased inp bt =
    isJustReleased $ Map.findWithDefault Up bt (buttons inp)

isMouseButtonJustPressed :: InputData -> SDL.MouseButton -> Bool
isMouseButtonJustPressed inp bt =
    isJustPressed $ Map.findWithDefault Up bt (mouseButtons . mouse $ inp)

isMouseButtonJustReleased :: InputData -> SDL.MouseButton -> Bool
isMouseButtonJustReleased inp bt =
    isJustReleased $ Map.findWithDefault Up bt (mouseButtons . mouse $ inp)

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

advanceButtonState :: ButtonState -> ButtonState
advanceButtonState JustPressed  = Down
advanceButtonState JustReleased = Up
advanceButtonState bs           = bs

modifyInput :: InputData -> SDL.Event -> InputData
modifyInput inpDat e =
    let newButtons =
            case SDL.eventPayload e of
                SDL.KeyboardEvent kbe ->
                    let motion = SDL.keyboardEventKeyMotion kbe -- pressed or released
                        keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym kbe)
                    in  modifyButton keycode (fromMotion motion)
                _ -> id
        newMouse =
            case SDL.eventPayload e of
                SDL.MouseMotionEvent mme -> \md ->
                    let (SDL.P mpos) = SDL.mouseMotionEventPos mme
                    in  md {mousePosition = fmap fromIntegral mpos}
                SDL.MouseButtonEvent mbe -> \md ->
                    let motion = SDL.mouseButtonEventMotion mbe
                        keycode = SDL.mouseButtonEventButton mbe
                    in  md {mouseButtons = modifyButton keycode
                                                        (fromMotion motion)
                                                        (mouseButtons md)}
                _ -> id
    in InputData (newButtons (buttons inpDat)) (newMouse (mouse inpDat))

fromMotion :: SDL.InputMotion -> ButtonState
fromMotion SDL.Pressed  = Down
fromMotion SDL.Released = Up

modifyButton :: (Ord a) => a -> ButtonState -> ButtonMap a -> ButtonMap a
modifyButton bt action = Map.alter (combine action) bt

advanceInputData :: InputData -> InputData
advanceInputData inpDat = InputData (Map.map advanceButtonState (buttons inpDat))
                                    ((mouse inpDat) {mouseButtons = Map.map advanceButtonState (mouseButtons (mouse inpDat))})
