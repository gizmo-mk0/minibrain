module Scene.Simulation.Render where
--
import qualified SDL

import Types (Vector2f, Rect2f(..))

import Render

import Scene.Simulation.Helper
import Scene.Simulation.Globals

renderSimulation :: SimulationData -> VectorImage
renderSimulation (SimulationData robot goal) =
    compound
        [ translate (robotPosition robot) $ fill robotColor $ circle 10
        , translate (goalPosition goal) $ fill goalColor $ circle 15 ]