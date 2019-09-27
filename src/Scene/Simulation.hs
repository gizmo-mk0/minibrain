module Scene.Simulation where

import qualified SDL

import Control.Varying.Core  (Var)
import Control.Varying.Event (Event)

import Scene (Scene)
import Input (InputEvent)

import Scene.Editor.Helper (EditorGraph (..))
import Scene.Simulation.Render (renderSimulation)
import Scene.Simulation.Input (mkNetwork)
import Scene.Simulation.Helper (SimulationData(..), Robot(..), Goal(..))

mkSimulation :: EditorGraph -> Var (Event InputEvent) Scene
mkSimulation g = mkSimulation' (SimulationData (Robot (SDL.V2 210 200) 0 g)
                               (Goal (SDL.V2 600 600)))
    where
    mkSimulation' :: SimulationData -> Var (Event InputEvent) Scene
    mkSimulation' sd = mkNetwork sd
