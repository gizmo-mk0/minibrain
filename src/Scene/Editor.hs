{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Scene.Editor where

--
import qualified SDL
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import Data.Maybe (fromJust, listToMaybe)
import Data.List (find)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)

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
                 , selectedNodes  :: [(Int, Vector2f)]
                 , selectedPin    :: Maybe (Int, (PinType, Int, Vector2f)) }
                --  , mousePressedAt :: Vector2f
                --  , currentTool    :: Maybe EditorTool }
                 deriving (Generic)

data PinType = InputPin | OutputPin

data EditorTool = Move | Select | Connect deriving (Eq)

defaultEditorData :: EditorData
defaultEditorData = testEditorData -- EditorData G.empty

testEditorData :: EditorData
testEditorData =
    EditorData
        ( G.insEdge (0, 1, Connection 0 0 0)
        $ G.insEdge (0, 3, Connection 1 0 0)
        $ G.insEdge (2, 3, Connection 0 1 0)
        $ G.insEdge (3, 4, Connection 0 0 0)
        $ G.insNode (4, Perceptron 2 2 (SDL.V2 400 200))
        $ G.insNode (3, Perceptron 2 1 (SDL.V2 200 200))
        $ G.insNode (2, Perceptron 3 3 (SDL.V2   0 200))
        $ G.insNode (1, Perceptron 2 3 (SDL.V2 200   0))
        $ G.insNode (0, Perceptron 3 2 (SDL.V2   0   0)) G.empty )
        Nothing
        []
        Nothing

getPerceptronRect :: Perceptron -> Rect2f
getPerceptronRect p =
    let h   = getPerceptronHeight p
        w   = perceptronWidth
        pos = position p
        halfSize = SDL.V2 (w / 2) (h / 2)
    in  Rect2f (pos - halfSize) (halfSize * 2)

getPinRects :: Perceptron -> [(PinType, Int, Vector2f)] -- pintype, pin num, pos
getPinRects p =
    let srcPins =
            map (\n -> (InputPin, n, getPinRelativePosition p n InputPin))
                [0..inputPinCount p]
        dstPins =
            map (\n -> (OutputPin, n, getPinRelativePosition p n OutputPin))
                [0..outputPinCount p]
        percPos = position p
    in  map (\(pt, pn, pinPos) -> (pt, pn, pinPos + percPos))
            (srcPins ++ dstPins)

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

getUnselectedNodes :: EditorData -> [Perceptron]
getUnselectedNodes EditorData{..} =
    map snd . filter ((`elem` (map fst selectedNodes)) . fst)
        $ (G.labNodes graph)

getSelectedNodes :: EditorData -> [Perceptron]
getSelectedNodes EditorData{..} =
    map snd . filter (not . (`elem` (map fst selectedNodes)) . fst)
        $ (G.labNodes graph)

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

nodesWithPosition :: (G.LNode Perceptron -> Bool) -> EditorGraph
                  -> [(Int, Vector2f)]
nodesWithPosition f = fmap (\(l, p) -> (l, position p)) . filter f . G.labNodes

getNodeAt :: Vector2f -> EditorGraph -> Maybe (Int, Vector2f)
getNodeAt p =
    listToMaybe . nodesWithPosition (pointInRect p . getPerceptronRect . snd)

moveNodeTo :: Vector2f -> Perceptron -> Perceptron
moveNodeTo v p@Perceptron{..} = p {position = v}

moveSelectedNodes :: EditorData -> Vector2f -> EditorGraph
moveSelectedNodes ed@EditorData{..} vec =
    G.gmap (\(p, v, l, s) ->
                if v `elem` (map fst selectedNodes)
                    then (p, v, moveNodeTo (newPos v vec) l, s)
                    else (p, v, l, s)) graph
    where
    newPos ix vec =
        (snd . fromJust . find ((== ix) . fst) $ selectedNodes) + vec

collectSelectedNodes :: EditorData -> [(Int, Vector2f)]
collectSelectedNodes EditorData{..} =
    case selectionRect of
        Nothing -> []
        Just sr -> nodesWithPosition ( doRectsIntersect sr
                                     . getPerceptronRect . snd) graph

updateSelectedNodes :: EditorData -> [(Int, Vector2f)]
updateSelectedNodes EditorData{..} =
    nodesWithPosition ((`elem` (map fst selectedNodes)) . fst) graph

getPinAt :: Vector2f -> EditorGraph -> Maybe (Int, (PinType, Int, Vector2f))
getPinAt p g =
    let nodes = G.labNodes g
        rects = concatMap (\(n, perc) -> zip (repeat n) (getPinRects perc))
                          nodes
        mkRect pinPos = rectAroundPosition pinPos (SDL.V2 pinWidth pinHeight)
    in  listToMaybe
        . filter (\(_, (_, _, pinPos)) -> pointInRect p (mkRect pinPos))
        $ rects

isNodeSelected :: EditorData -> Int -> Bool
isNodeSelected ed n = n `elem` (fmap fst $ selectedNodes ed)