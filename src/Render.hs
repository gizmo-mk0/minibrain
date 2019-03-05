module Render where

--
import qualified Data.IntMap as Map
import qualified SDL
import qualified Graphics.Gloss.Rendering as G

import Control.Monad.State.Strict (gets)
import Control.Monad.Reader (asks, liftIO)
import Control.Lens ((^.))
import Linear.Metric (norm)

import Types
import Globals
import GameData
import Scene

isPointInPoly :: [Vector2f] -> Vector2f -> Bool
isPointInPoly poly p@(SDL.V2 x y) =
    let minX = minimum (map (^.SDL._x) poly)
        maxX = maximum (map (^.SDL._x) poly)
        minY = minimum (map (^.SDL._y) poly)
        maxY = maximum (map (^.SDL._y) poly)
        sides = zip poly (drop 1 (cycle poly))
        isEven = (== 0) . (`mod` 2)
    in  if x < minX || x > maxX || y < minY || y > maxY
            then False
            else not . isEven . length
               . filter (doIntersect ((SDL.V2 (minX - 1) y), p))
               $ sides

lineToPoly :: Float -> [Vector2f] -> [[Vector2f]]
lineToPoly _ []     = error "lineToPoly called with empty list"
lineToPoly _ (_:[]) = error "lineToPoly called with one element"
lineToPoly thickness line' =
    let halfThickness = thickness / 2
        duplicateFirst (x1:x2:xs) = (2 * x1 - x2):x1:x2:xs
        line = reverse . duplicateFirst . reverse . duplicateFirst $ line'
        normals = map (fmap (* halfThickness) . SDL.signorm . SDL.perp)
                . zipWith (-) line $ (drop 1 line)
        avgNormals  = map (fmap (/2)) . zipWith (+) normals . drop 1 $ normals
        leftPoints  = zipWith (+) line' avgNormals
        rightPoints = zipWith (-) line' avgNormals
        pointPairs  = zip leftPoints rightPoints
        pointQuads  = zip pointPairs (drop 1 pointPairs)
    in  map (\((p1, p2), (p4, p3)) -> [p1, p2, p3, p4]) pointQuads

-- does the infinite horizontal line, starting from p, intersect the p1-p2 line
-- segment?
doIntersect :: (Vector2f, Vector2f) -> (Vector2f, Vector2f) -> Bool
doIntersect (SDL.V2 v1x1 v1y1, SDL.V2 v1x2 v1y2)
            (SDL.V2 v2x1 v2y1, SDL.V2 v2x2 v2y2) =
    let a1 = v1y2 - v1y1
        b1 = v1x1 - v1x2
        c1 = (v1x2 * v1y1) - (v1x1 * v1y2)
        d1 = (a1 * v2x1) + (b1 * v2y1) + c1
        d2 = (a1 * v2x2) + (b1 * v2y2) + c1
        a2 = v2y2 - v2y1
        b2 = v2x1 - v2x2
        c2 = (v2x2 * v2y1) - (v2x1 * v2y2)
        d3 = (a2 * v1x1) + (b2 * v1y1) + c2
        d4 = (a2 * v1x2) + (b2 * v1y2) + c2
    in  if (d1 > 0 && d2 > 0) || (d1 < 0 && d2 < 0)
            then False
            else if (d3 > 0 && d4 > 0) || (d3 < 0 && d4 < 0)
                then False
                else True

bezier :: [Vector2f] -> Float -> Vector2f
bezier vs t | length vs == 4 = fmap (* ((1 - t) ** 3))           (vs !! 0)
                             + fmap (* (3 * ((1 - t) ** 2) * t)) (vs !! 1)
                             + fmap (* (3 * (1 - t) * (t ** 2))) (vs !! 2)
                             + fmap (* (t ** 3))                 (vs !! 3)
            | otherwise = error "bezier called with fewer or more than 4 points"

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

-- thickCurve :: Float -> [Vector2f] -> G.Picture
-- thickCurve thickness points = G.Polygon . map (\(SDL.V2 x y) -> (x, y))
--                                         $ lineToPoly thickness $ points

thickCurve :: Float -> [Vector2f] -> G.Picture
thickCurve thickness points = G.Pictures
                            . map (G.Polygon . map (\(SDL.V2 x y) -> (x, y)))
                            . lineToPoly thickness
                            $ points

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
            yOffset = if x2 < x1 then (-xDelta / 2) else 0
            pos3 = SDL.V2 (max xMid (x1 + xDelta)) (y1 + yOffset)
            pos4 = SDL.V2 (min xMid (x2 - xDelta)) (y2 + yOffset)
            points = map (bezier [pos1, pos3, pos4, pos2]) (map (/20) [0..20])
        in G.Color pinColor $
                thickCurve connectionWidth points
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