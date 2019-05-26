module Scene.Simulation.Stepper where

import qualified Data.Graph.Inductive.Graph as G
import qualified SDL

import Types (Vector2f)
import Utils (clamp)

import Scene.Editor.Helper (EditorGraph, Perceptron(..))

import Scene.Simulation.Helper

step :: SimulationData -> SimulationData
step sd@(SimulationData robot goal) = SimulationData (updateRobot sd robot) goal

updateRobot :: SimulationData -> Robot -> Robot
updateRobot sd robot@(Robot pos@(SDL.V2 px py) dir brain) =
    let speed    = getOutput "move" brain
        dirDelta = getOutput "rotate" brain
        newDir   = dir + dirDelta
        vel      = SDL.V2 (cos newDir) (sin newDir)
        newPos   = pos + vel * 0.01
        newBrain = updateBrain sd robot brain
    -- in  Robot newPos newDir newBrain
    in  Robot (SDL.V2 (px + 10) py) newDir newBrain

updateBrain :: SimulationData -> Robot -> EditorGraph -> EditorGraph
updateBrain sd robot =
    let distance = measureDistance (goalPosition . goal $ sd)
                                   (robotPosition robot)
        direction = measureDirection (goalPosition . goal $ sd)
                                     (robotPosition robot)
    in  setInput "goalDirection" (direction / pi) .
            setInput "goalDistance" (clamp 0 1 (distance / 100))

getOutput :: String -> EditorGraph -> Float
getOutput lab = pvalue . snd . head . G.labNodes
              . G.labfilter ((== lab) . label)

setInput :: String -> Float -> EditorGraph -> EditorGraph
setInput lab val = G.nmap updateValue
    where
    updateValue :: Perceptron -> Perceptron
    updateValue p | label p == lab = p {pvalue = val}
                  | otherwise      = p

measureDirection :: Vector2f -> Vector2f -> Float
measureDirection (SDL.V2 x1 y1) (SDL.V2 x2 y2) = atan2 (y2 - y1) (x2 - x1)

measureDistance :: Vector2f -> Vector2f -> Float
measureDistance (SDL.V2 x1 y1) (SDL.V2 x2 y2) =
    let xDelta = x2 - x1
        yDelta = y2 - y1
    in  (sqrt xDelta * xDelta + yDelta * yDelta)