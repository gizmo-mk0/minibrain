{-# LANGUAGE DuplicateRecordFields #-}

module Scene.Editor where

--
import qualified SDL
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import Data.Maybe (fromJust)
import Foreign.C.Types (CInt)

import Types
import Globals

type EditorGraph = G.Gr Perceptron Connection

-- The Perceptron stores the number of available pins and its own position
data Perceptron  = Perceptron
                 { inputPinCount  :: Int
                 , outputPinCount  :: Int
                 , position     :: Vector2 }

-- The Connection stores which pin is connected to which pin
data Connection  = Connection
                 { srcPinNumber :: Int
                 , dstPinNumber :: Int
                 , gain         :: Float }

data EditorData  = EditorData
                 { graph        :: EditorGraph }

data PinType = InputPin | OutputPin

defaultEditorData :: EditorData
defaultEditorData = testEditorData -- EditorData G.empty

testEditorData :: EditorData
testEditorData =
    EditorData
        $ G.insEdge (0, 1, Connection 0 0 0)
        $ G.insNode (1, Perceptron 3 2 (SDL.V2 300 100))
        $ G.insNode (0, Perceptron 3 2 (SDL.V2 100 100)) G.empty

getPerceptronHeight :: Perceptron -> CInt
getPerceptronHeight n =
    perceptronBodyRoundness * 2 + moduleCount * perceptronModuleHeight
    where
    moduleCount = fromIntegral $ max (inputPinCount n) (outputPinCount n)

nodes :: EditorData -> [Perceptron]
nodes (EditorData g) = map snd (G.labNodes g)

edges :: EditorData -> [(Perceptron, Perceptron, Connection)]
edges (EditorData g) = map getWithLabels (G.labEdges g)
        where
        getWithLabels :: (G.Node, G.Node, Connection)
                      -> (Perceptron, Perceptron, Connection)
        getWithLabels (n1, n2, c) =
            (fromJust $ G.lab g n1, fromJust $ G.lab g n2, c)

getPinPosition :: Perceptron -> Int -> PinType -> Vector2
getPinPosition p n t =
    let verticalPos  = -(parentHeight `div` 2)
                     + perceptronBodyRoundness
                     + (fromIntegral n * perceptronModuleHeight)
                     + (perceptronModuleHeight `div` 2)
        parentHeight = getPerceptronHeight p
    in  case t of
        InputPin  -> position p - (SDL.V2 (perceptronWidth `div` 2) (-verticalPos))
        OutputPin -> position p + (SDL.V2 (perceptronWidth `div` 2) verticalPos)