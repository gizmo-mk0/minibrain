module Scene.Simulation.Helper where

import Types (Vector2f)

import Scene.Editor.Helper (EditorGraph)

data SimulationData = SimulationData
                    { robot :: Robot
                    , goal  :: Goal }

data Robot = Robot
           { robotPosition    :: Vector2f
           , robotOrientation :: Float
           , brain            :: EditorGraph }

data Goal = Goal
          { goalPosition :: Vector2f }

