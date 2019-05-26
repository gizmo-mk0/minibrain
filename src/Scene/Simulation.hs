module Scene.Simulation where

import qualified SDL

import Scene (Scene, mkScene)

import Scene.Editor.Helper (EditorGraph (..))
import Scene.Simulation.Render (renderSimulation)
import Scene.Simulation.Input (mkNetwork)
import Scene.Simulation.Helper (SimulationData(..), Robot(..), Goal(..))

mkSimulation :: EditorGraph -> Scene
mkSimulation g = mkSimulation' (SimulationData (Robot (SDL.V2 10 10) 0 g)
                               (Goal (SDL.V2 400 400)))
    where
    mkSimulation' :: SimulationData -> Scene
    mkSimulation' sd = mkScene update render
        where
        update = (fmap . fmap . fmap) mkSimulation' . mkNetwork sd
        render = renderSimulation sd
