{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Scene where

import qualified SDL

import Control.Event.Handler      (AddHandler)
import Data.IORef                 (IORef, modifyIORef)
import Data.Maybe                 (fromJust)
import Reactive.Banana.Frameworks (MomentIO, fromAddHandler, reactimate)

import Reactive.Banana

import Input  (InputEvent(..))
import Types  (Vector2f)
import Render (VectorImage, blank)
import Input  (InputData(..))

import Utils

data Scene = Scene
           { update :: InputData -> MomentIO (Event (StackCommand, Scene))
           , render :: VectorImage }

mkScene :: (InputData -> MomentIO (Event (StackCommand, Scene)))
        -> VectorImage -> Scene
mkScene updateF renderF = Scene updateF renderF

emptyScene = mkScene (const $ return never) blank

data StackCommand = None          -- No scene change
                  | Done          -- Current scene is done, remove from stack
                  | Push Scene    -- Add scene to the stack
                  | Replace Scene -- Replace current scene with another scene
                --   | Quit          -- Remove all scenes from stack

sceneNetwork :: Stack Scene -> IORef (Stack Scene)
             -> (AddHandler InputEvent, AddHandler ()) -> MomentIO ()
sceneNetwork stack sref (inp, frame) = mdo
    events      <- accumE (MouseMoveEvent (SDL.V2 0 0))
                        (unions [fmap const inputEvents, id <$ frameEvents])
    inputEvents <- fromAddHandler inp
    frameEvents <- fromAddHandler frame
    mousePosB   <- stepper (SDL.V2 0 0) mouseMoveEvents
    let mouseMoveEvents :: Event Vector2f
        mouseMoveEvents = fmap (\(MouseMoveEvent x) -> x)
                        . filterE (\case MouseMoveEvent _ -> True;
                                         _ -> False)
                        $ events
    sceneUpdate <- (update (top stack)) (InputData events mousePosB)
    reactimate $ fmap (modifyIORef sref . uncurry updateStack) sceneUpdate

-- | Update the top state on the stack
updateStack :: StackCommand -> Scene -> Stack Scene -> Stack Scene
updateStack cmd newScene stack =
    case cmd of
        None      -> replace newScene stack
        Done      -> fromJust $ snd (pop stack)
        Replace s -> replace s stack
        Push s    -> push s (replace newScene stack)