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
import Data.Graph.AStar
import qualified Data.HashSet as H

import Types
import Globals

type EditorGraph = G.Gr Perceptron Connection

type NodeIndex = Int
type PinIndex = Int
type PinInfo = (NodeIndex, (PinType, PinIndex, Vector2f))

-- The Perceptron stores the number of available pins and its own position
data Perceptron  = Perceptron
                 { inputPinCount  :: Int
                 , outputPinCount :: Int
                 , position       :: Vector2f }

-- The Connection stores which pin is connected to which pin
data Connection  = Connection
                 { srcPinNumber :: PinIndex
                 , dstPinNumber :: PinIndex
                 , gain         :: Float }

data EditorData  = EditorData
                 { graph          :: EditorGraph
                 , selectionRect  :: Maybe Rect2f
                 , selectedNodes  :: [(NodeIndex, Vector2f)]
                 , selectedPin    :: Maybe PinInfo }
                --  , mousePressedAt :: Vector2f
                --  , currentTool    :: Maybe EditorTool }
                 deriving (Generic)

data PinType = InputPin | OutputPin deriving (Eq)

otherPin :: PinType -> PinType
otherPin InputPin = OutputPin
otherPin OutputPin = InputPin

data EditorTool = Move | Select | Connect | Tune deriving (Eq)

defaultEditorData :: EditorData
defaultEditorData = testEditorData -- EditorData G.empty

testEditorData :: EditorData
testEditorData =
    EditorData
        ( G.insEdge (0, 1, Connection 0 0 0)
        $ G.insEdge (0, 3, Connection 0 0 0)
        $ G.insEdge (2, 3, Connection 0 0 0)
        $ G.insEdge (3, 4, Connection 0 0 0)
        $ add (SDL.V2 400 0)
        $ add (SDL.V2 200 0)
        $ add (SDL.V2 0 0)
        $ add (SDL.V2 200 200)
        $ add (SDL.V2 0 200) G.empty )
        Nothing
        []
        Nothing
    where
    add = flip addNodeAt

getPerceptronRect :: Perceptron -> Rect2f
getPerceptronRect p =
    let h   = getPerceptronHeight p
        w   = perceptronWidth
        pos = position p
        halfSize = SDL.V2 (w / 2) (h / 2)
    in  Rect2f (pos - halfSize) (halfSize * 2)

getPerceptronGridSize :: Perceptron -> Rect2f
getPerceptronGridSize p =
    let h   = getPerceptronHeight p + fromIntegral editorGridSize
        w   = perceptronWidth + fromIntegral editorGridSize
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

addNodeAt :: EditorGraph -> Vector2f -> EditorGraph
addNodeAt g p = G.insNode (ix, perc) g
    where
    ix = if G.isEmpty g then 0 else snd (G.nodeRange g) + 1
    perc = Perceptron 1 1 p

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
        InputPin  -> (SDL.V2 (- (pinWidth / 2) - perceptronWidth / 2) verticalPos)
        OutputPin -> (SDL.V2 ((pinWidth / 2) + perceptronWidth / 2) verticalPos)

getPinAbsolutePosition :: Perceptron -> Int -> PinType -> Vector2f
getPinAbsolutePosition p n t = position p + getPinRelativePosition p n t

nodesWithPosition :: (G.LNode Perceptron -> Bool) -> EditorGraph
                  -> [(Int, Vector2f)]
nodesWithPosition f = fmap (\(l, p) -> (l, position p)) . filter f . G.labNodes

getNodeAt :: Vector2f -> EditorGraph -> Maybe (Int, Vector2f)
getNodeAt p =
    listToMaybe . nodesWithPosition (pointInRect p . getPerceptronRect . snd)

isOccupied :: Vector2f -> EditorGraph -> Bool
isOccupied p =
    not . null . nodesWithPosition (pointInRect p . getPerceptronGridSize . snd)
    
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

deleteSelectedNodes :: [NodeIndex] -> EditorGraph -> EditorGraph
deleteSelectedNodes ixs g = G.delNodes ixs g

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

-- TODO check if this connection is not already in the graph
connect :: EditorGraph -> (PinInfo, PinInfo) -> EditorGraph
connect graph ((n1, (pt1, _, _)), (n2, (pt2, _, _))) =
    if pt1 /= pt2
        then if pt1 == OutputPin
            then G.insEdge (n1, n2, Connection 0 0 0) graph
            else G.insEdge (n2, n1, Connection 0 0 0) graph
        else graph

-- snapGraph :: EditorGraph -> EditorGraph
-- snapGraph = G.nmap (\p -> p {position = snapTo editorGridSize (position p)})

graphSize :: EditorGraph -> Rect2f
graphSize graph = Rect2f topLeft (bottomRight - topLeft)
    where
    firstNodePos = position . snd. head . G.labNodes $ graph
    topLeft     = G.ufold (\(_, _, perc, _) p ->
                                selectCorner min (position perc) p)
                          firstNodePos graph
    bottomRight = G.ufold (\(_, _, perc, _) p ->
                                selectCorner max (position perc) p)
                          firstNodePos graph
    selectCorner :: (a -> a -> a) -> (SDL.V2 a) -> (SDL.V2 a) -> (SDL.V2 a)
    selectCorner f (SDL.V2 x1 y1) (SDL.V2 x2 y2) = SDL.V2 x y
        where
        x = f x1 x2
        y = f y1 y2
