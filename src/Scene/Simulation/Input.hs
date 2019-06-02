{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Scene.Simulation.Input where

import qualified SDL

import Control.Varying.Core  ( Var, arr, (>>>), mkState, VarT(..), runVarT )
import Control.Varying.Event ( Event, startWith, onlyWhenE, onTrue, onWhen
                             , use, anyE, foldStream, onUnique, filterE )

import Scene    (StackCommand(..), Scene(..))
import Types    (Vector2f, Rect2f(..))
import Input    (InputEvent(..))

import Scene.Simulation.Helper
import Scene.Simulation.Globals
import Scene.Simulation.Stepper (step)
import Scene.Simulation.Render  (renderSimulation)

-- mkNetwork :: EditorData -> Var (Event InputEvent) Scene
-- mkNetwork ed' = (,) <$> selectionRectArr <*> cmdArr
--             >>> mkState (\(rect, cmd) ed ->
--                             ( Scene (cmd (graph ed))
--                                     (renderEditor ed {selectionRect = rect})
--                             , ed ))
--                         ed'

mkNetwork :: SimulationData -> Var (Event InputEvent) Scene
mkNetwork sd' = cmdArr
            >>> mkState (\cmd sd ->
                           (Scene cmd (renderSimulation sd), step sd))
                        sd'
    where
    cmdArr = use Done (pressedEsc >>> onTrue) >>> startWith None

pressedEsc :: Var (Event InputEvent) Bool
pressedEsc =
    foldStream (\v ->
        (\case KeyboardEvent SDL.KeycodeEscape SDL.Pressed  -> True
               KeyboardEvent SDL.KeycodeEscape SDL.Released -> False
               _ -> v))
        False

-- mkNetwork :: SimulationData -> InputData
--           ->  MomentIO (Event (StackCommand, SimulationData))
-- mkNetwork sd' (InputData events mousePosB) = mdo
--     stackCommandB <- stepper None (pure Done <@ pressedEscE)
--     simulationDataB <- stepper sd' (fmap step simulationDataB <@ events)
--     let retVal = (,) <$> stackCommandB <*> simulationDataB
--         pressedSpaceE :: Event InputEvent
--         pressedSpaceE =
--             filterE (== KeyboardEvent SDL.KeycodeSpace SDL.Pressed) events
--         pressedEscE :: Event InputEvent
--         pressedEscE =
--             filterE (== KeyboardEvent SDL.KeycodeEscape SDL.Pressed) events
--     return (retVal <@ events)

