{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Scene.Simulation.Input where

import qualified SDL

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks (MomentIO)

import Scene    (StackCommand(..))
import Types    (Vector2f, Rect2f(..))
import Input    (InputEvent(..), InputData(..))

import Scene.Simulation.Helper
import Scene.Simulation.Globals
import Scene.Simulation.Stepper (step)

mkNetwork :: SimulationData -> InputData
          ->  MomentIO (Event (StackCommand, SimulationData))
mkNetwork sd' (InputData events mousePosB) = mdo
    stackCommandB <- stepper None (pure Done <@ pressedEscE)
    simulationDataB <- stepper sd' (fmap step simulationDataB <@ events)
    let retVal = (,) <$> stackCommandB <*> simulationDataB
        pressedSpaceE :: Event InputEvent
        pressedSpaceE =
            filterE (== KeyboardEvent SDL.KeycodeSpace SDL.Pressed) events
        pressedEscE :: Event InputEvent
        pressedEscE =
            filterE (== KeyboardEvent SDL.KeycodeEscape SDL.Pressed) events
    return (retVal <@ events)

