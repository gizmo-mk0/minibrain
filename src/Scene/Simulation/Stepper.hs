module Scene.Simulation.Stepper where

import qualified Data.Graph.Inductive.Graph as G
import qualified SDL

import Types      ( Vector2f )
import Utils      ( clamp )
import Data.Maybe ( fromJust )
import Data.List  ( (\\) )
import Data.Fixed ( mod' )

import Scene.Editor.Helper ( EditorGraph, Perceptron(..), Connection(..)
                           , NodeIndex )

import Scene.Simulation.Helper

import Debug.Trace

step :: SimulationData -> SimulationData
step sd@(SimulationData robot goal) = SimulationData (updateRobot sd robot) goal

-- NOTE: The reason robot brains have state is because it is possible to create
-- circular dependence on perceptrons. If there was no state maintained, it
-- would create a race condition.
updateRobot :: SimulationData -> Robot -> Robot
updateRobot sd robot@(Robot pos dir brain) =
    let speed    = getOutput "move" brain * 0.01
        dirDelta = getOutput "rotate" brain * 0.1
        newDir   = dir + dirDelta * 0.01
        vel      = SDL.V2 (cos newDir) (sin newDir)
        newPos   = pos + vel * (SDL.V2 speed speed)
        newBrain = updateBrain sd robot
    in  Robot newPos newDir newBrain

updateBrain :: SimulationData -> Robot -> EditorGraph
updateBrain sd robot = 
    let distance = measureDistance (robotPosition robot)
                                   (goalPosition . goal $ sd)
        direction = measureDirection (robotPosition robot)
                                     (goalPosition . goal $ sd)
    in  updatePerceptrons
        . setInput "foodDirection"
            (directionDifference (robotOrientation robot / pi)
                                 (direction / pi))
        . setInput "foodDistance" (clamp 0 1 (distance / 100))
        . brain $ robot

updatePerceptrons :: EditorGraph -> EditorGraph
updatePerceptrons g = updatePerceptrons' [] inputs g
    where
    inputs = G.nodes $ G.nfilter ((> 0). G.outdeg g) g
    updatePerceptrons' :: [NodeIndex] -> [NodeIndex] -> EditorGraph
                       -> EditorGraph
    updatePerceptrons' _ [] g = g
    updatePerceptrons' visited toUpdate g =
        let mkNewGraph  = G.gmap $ \(p, v, l, s) ->
                            case v `elem` toUpdate && v `notElem` inputs of
                            True  -> ( p, v
                                     , l {pvalue = newPerceptronValue v g}
                                     , s)
                            False -> (p, v, l, s)
            newVisited  = visited ++ toUpdate
            newToUpdate = (concatMap (G.suc g) toUpdate) \\ newVisited
        in  updatePerceptrons' newVisited newToUpdate (mkNewGraph g)
    newPerceptronValue :: NodeIndex -> EditorGraph -> Float
    newPerceptronValue n g =
        -- sum up all the values multiplied by their corresponding edges
        foldr (\(v, c) s -> s + (v * c)) 0
        -- get the perceptron value from the node label and the connection value
        -- from the connection label
        . map (\(ni, c) -> let x = fromJust $ G.lab g ni in
                            (pvalue x + baseLevel x, gain c))
        -- get all incoming neighbours and the connection labels as pairs
        $ G.lpre g n

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
    in  sqrt (xDelta * xDelta + yDelta * yDelta)

normalizeDirection :: Float -> Float
normalizeDirection dir = ((dir + pi) `mod'` (2 * pi)) - pi

directionDifference :: Float -> Float -> Float
directionDifference dir1 dir2 =
    let dirMin = min (normalizeDirection dir1) (normalizeDirection dir2)
        d1 = dir1 - dirMin
        d2 = dir2 - dirMin
    in  normalizeDirection (d1 - d2)