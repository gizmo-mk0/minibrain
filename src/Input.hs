{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Input
    ( module X
    , module Input )
    where
--

import qualified SDL

import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Data.IORef

import GameData
import Types

import Input.Editor as X

sceneNetwork :: Config -> GameData -> IORef GameData
             -> (AddHandler InputEvent, AddHandler ()) -> MomentIO ()
sceneNetwork cfg gd gdref (inp, frame) = mdo
    events      <- accumE (MouseMoveEvent (SDL.V2 0 0))
                        (unions [fmap const inputEvents, id <$ frameEvents])
    inputEvents <- fromAddHandler inp
    frameEvents <- fromAddHandler frame
    mousePosB   <- stepper (SDL.V2 0 0)
                $ fmap (\(cData, x) -> convertPoint cData x)
                $ (fmap (\cd' -> (,) cd') cameraDataB) <@> mouseMoveEvents
    let quitE = filterE (== KeyboardEvent SDL.KeycodeEscape SDL.Pressed) events
        cameraDataB = pure (cameraData gd)
        convertPoint cameraData p =
            toWorldCoords cameraData (getWindowSize cfg) p
        mouseMoveEvents :: Event Vector2f
        mouseMoveEvents = fmap (\(MouseMoveEvent x) -> x)
                        . filterE (\case MouseMoveEvent _ -> True;
                                         _ -> False)
                        $ events
    newGameData <- editorNetwork gd events mousePosB cameraDataB
    reactimate $ fmap (writeIORef gdref) newGameData
