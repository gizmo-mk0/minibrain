module Render where

--
import qualified Data.IntMap as Map
import qualified SDL
import qualified Graphics.Gloss.Rendering as G

import Control.Monad.State.Strict (gets)
import Control.Monad.Reader (asks, liftIO)

import Types
import Globals
import GameData
import Scene

bezier :: [Vector2f] -> Float -> Vector2f
bezier vs t | length vs == 4 = fmap (* ((1 - t) ** 3))       (vs !! 0)
                             + fmap (* (3*((1 - t) ** 2) * t)) (vs !! 1)
                             + fmap (* (3*(1 - t) * (t ** 2))) (vs !! 2)
                             + fmap (* (t ** 3))             (vs !! 3)
            | otherwise = error "bezier called with less or more than 4 points"

circleSolid :: Float -> G.Picture
circleSolid r = G.ThickCircle (r / 2) r

thickLine :: Vector2f -> Vector2f -> Float -> G.Picture
thickLine start@(SDL.V2 x1 y1) end@(SDL.V2 x2 y2) w = G.Polygon points
    where
    (SDL.V2 x y) = end - start
    normal = SDL.V2 (-y * (w / 2) / l) (x * (w / 2) / l)
    l = sqrt (x * x + y * y)
    points = map (\(SDL.V2 c1 c2) -> (c1, c2))
                 [start + normal, end + normal, end - normal, start - normal]

fillRoundRectangle :: Vector2f -> Float -> G.Picture
fillRoundRectangle (SDL.V2 w h) r =
    G.Pictures $ G.Polygon points
               : map (\(dx, dy) -> G.Translate dx dy (circleSolid r))
                     circlePositions
    where
    points = [ (-(w / 2) + r, -(h / 2))
             , ( (w / 2) - r, -(h / 2))
             , ( (w / 2),     -(h / 2) + r)
             , ( (w / 2),      (h / 2) - r)
             , ( (w / 2) - r,  (h / 2))
             , (-(w / 2) + r,  (h / 2))
             , (-(w / 2),      (h / 2) - r)
             , (-(w / 2),     -(h / 2) + r)]
    circlePositions = [ (-(w / 2) + r, -(h / 2) + r)
                      , ( (w / 2) - r, -(h / 2) + r)
                      , (-(w / 2) + r,  (h / 2) - r)
                      , ( (w / 2) - r,  (h / 2) - r) ]

renderCurrentScene :: Config -> GameData -> IO ()
renderCurrentScene (Config window (SDL.V2 w h) s)
                   (GameData sd (CameraData (SDL.V2 dx dy) rotation zoom) md)
                   = do
    let scene         = currentScene sd
        sceneGeometry =
            case scene of
                Title      -> renderTitle
                Briefing   -> renderBriefing
                Editor     -> renderEditor md
                Simulation -> renderSimulation
                Quit       -> return G.Blank
    -- TODO make zoom relative to the screen center instead of the world center
    liftIO $ G.displayPicture (w, h) (G.makeColor 0 0 0 0) s zoom
           $ G.Translate dx dy
           $ G.Rotate rotation
           $ G.Scale zoom zoom
           $ sceneGeometry
           $ sd
    SDL.glSwapWindow window

renderTitle :: SceneData -> G.Picture
renderTitle = undefined

renderBriefing :: SceneData -> G.Picture
renderBriefing = undefined

renderEditor :: Vector2f -> SceneData -> G.Picture
renderEditor md sd =
    let perceptrons         = (getUnselectedNodes . editorData) sd
        selectedPerceptrons = (getSelectedNodes . editorData) sd
        connections         = (edges . editorData) sd
        selection           = (selectionRect . editorData) sd
        selectionShape =
            case selection of
                Nothing -> G.Blank
                Just selRect -> renderSelection selRect
        activePin = (selectedPin . editorData) sd
    in  G.Color background
        --    $ renderBackground
           $ G.Pictures $ map (renderPerceptron True)  selectedPerceptrons
                       ++ map renderConnection connections
                       ++ map (renderPerceptron False) perceptrons
                       ++ [selectionShape, connectionToolLine md activePin]
    where
    renderPerceptron :: Bool -> Perceptron -> G.Picture
    renderPerceptron s p =
        let w            = perceptronWidth
            h            = getPerceptronHeight p
            size         = SDL.V2 w h
            (SDL.V2 x y) = position p
            body = G.Color (if s then perceptronBodyColor
                                 else perceptronSelectedBodyColor)
                            $ fillRoundRectangle size perceptronBodyRoundness
            pins = map (renderPin p)
                       (zip [0..inputPinCount p - 1] (repeat InputPin) ++
                       zip [0..outputPinCount p - 1] (repeat OutputPin))
        in  G.Translate x y $ G.Pictures (reverse $ body : pins)
    renderPin :: Perceptron -> (Int, PinType) -> G.Picture
    renderPin perc (n, t) =
        let (SDL.V2 px py) = getPinRelativePosition perc n t
            size           = SDL.V2 pinWidth pinHeight
        in G.Translate px py $ G.Color pinColor $ fillRoundRectangle size 0
    -- renderConnection :: (Perceptron, Perceptron, Connection) -> G.Picture
    -- renderConnection (p1, p2, c) =
    --     let pos1 = getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
    --         pos2 = getPinAbsolutePosition p2 (dstPinNumber c) InputPin
    --     in G.Color pinColor $ thickLine pos1 pos2 connectionWidth
    renderConnection :: (Perceptron, Perceptron, Connection) -> G.Picture
    renderConnection (p1, p2, c) =
        let pos1@(SDL.V2 x1 y1) = getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
            pos2@(SDL.V2 x2 y2) = getPinAbsolutePosition p2 (dstPinNumber c) InputPin
            xMid = (x1 + x2) / 2
            xDelta = abs (x1 - x2)
            pos3 = SDL.V2 (max xMid (x1 + xDelta)) y1
            pos4 = SDL.V2 (min xMid (x2 - xDelta)) y2
            points = map (bezier [pos1, pos3, pos4, pos2]) (map (/20) [0..20])
        in G.Color pinColor $
                G.Pictures $
                    (map (\(start, end) -> thickLine start end connectionWidth) (zip points (drop 1 points)))
                    -- ++ [ thickLine pos1 pos3 connectionWidth
                    --    , thickLine pos3 pos4 connectionWidth
                    --    , thickLine pos4 pos2 connectionWidth]
        
    -- TODO
    -- renderBackground :: Minibrain ()
    -- renderBackground = undefined
    renderSelection :: Rect2f -> G.Picture
    renderSelection (Rect2f (SDL.V2 left top) (SDL.V2 w h)) =
        let points =
                [ (left,       top)
                , ((left + w), top)
                , ((left + w), (top + h))
                , (left,       (top + h)) ]
            coords = zip points (drop 1 (cycle points))
        in  G.Pictures
                [ G.Color selectionLineColor $ G.Pictures $
                        map (\((p1x, p1y), (p2x, p2y)) ->
                                thickLine (SDL.V2 p1x p1y)
                                          (SDL.V2 p2x p2y) 1) coords
                , G.Color selectionFillColor $ G.Polygon points ]
    connectionToolLine :: Vector2f -> Maybe (Int, (PinType, Int, Vector2f))
                       -> G.Picture
    connectionToolLine md (Just (_, (_, _, pp))) =
        G.Color pinColor $ thickLine md pp connectionWidth
    connectionToolLine _ _ = G.Blank

renderSimulation :: SceneData -> G.Picture
renderSimulation = undefined