{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

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
                 , outputPinCount :: Int
                 , position       :: Vector2f }

-- The Connection stores which pin is connected to which pin
data Connection  = Connection
                 { srcPinNumber :: Int
                 , dstPinNumber :: Int
                 , gain         :: Float }

data EditorData  = EditorData
                 { graph          :: EditorGraph
                 , selectionRect  :: Maybe Rect2f
                 , selectedNodes  :: [Int]
                 , mousePressedAt :: Vector2f }

data PinType = InputPin | OutputPin

defaultEditorData :: EditorData
defaultEditorData = testEditorData -- EditorData G.empty

testEditorData :: EditorData
testEditorData =
    EditorData
        ( G.insEdge (0, 1, Connection 0 1 0)
        $ G.insNode (1, Perceptron 3 2 (SDL.V2 300 100))
        $ G.insNode (0, Perceptron 3 2 (SDL.V2 100 100)) G.empty )
        Nothing
        []
        (SDL.V2 0 0)

getPerceptronHeight :: Perceptron -> Float
getPerceptronHeight n =
    perceptronBodyRoundness * 2 + moduleCount * perceptronModuleHeight
    where
    moduleCount = fromIntegral $ max (inputPinCount n) (outputPinCount n)

addPerceptron :: EditorData -> Perceptron -> EditorData
addPerceptron e p =
    e {graph = G.insNode ((+1) . snd $ G.nodeRange (graph e), p) (graph e)}

nodes :: EditorData -> [Perceptron]
nodes EditorData{..} = map snd (G.labNodes graph)

edges :: EditorData -> [(Perceptron, Perceptron, Connection)]
edges EditorData{..} = map getWithLabels (G.labEdges graph)
        where
        getWithLabels :: (G.Node, G.Node, Connection)
                      -> (Perceptron, Perceptron, Connection)
        getWithLabels (n1, n2, c) =
            (fromJust $ G.lab graph n1, fromJust $ G.lab graph n2, c)

getPinRelativePosition :: Perceptron -> Int -> PinType -> Vector2f
getPinRelativePosition p n t =
    let verticalPos  = (parentHeight / 2)
                     - perceptronBodyRoundness
                     - (fromIntegral n * perceptronModuleHeight)
                     - (perceptronModuleHeight / 2)
        parentHeight = getPerceptronHeight p
    in  case t of
        InputPin  -> (SDL.V2 (-perceptronWidth / 2) verticalPos)
        OutputPin -> (SDL.V2 ( perceptronWidth / 2) verticalPos)

getPinAbsolutePosition :: Perceptron -> Int -> PinType -> Vector2f
getPinAbsolutePosition p n t = position p + getPinRelativePosition p n t
