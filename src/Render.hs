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

renderCurrentScene :: Minibrain ()
renderCurrentScene = do
    (SDL.V2 w h)   <- asks getWindowSize
    s              <- asks getGlossState
    window         <- asks getWindow
    zoom           <- gets (cZoom . cameraData)
    rotation       <- gets (cRotation . cameraData)
    (SDL.V2 dx dy) <- gets (cPosition . cameraData)
    scene          <- gets (currentScene . sceneData)
    sceneGeometry <-
        case scene of
            Title      -> renderTitle
            Briefing   -> renderBriefing
            Editor     -> renderEditor
            Simulation -> renderSimulation
            Quit       -> return G.Blank
    liftIO $ G.displayPicture (w, h) (G.makeColor 0 0 0 0) s zoom
           $ G.Translate dx dy
           $ G.Rotate rotation
           $ G.Scale zoom zoom
           $ sceneGeometry
    SDL.glSwapWindow window

renderTitle :: Minibrain G.Picture
renderTitle = undefined

renderBriefing :: Minibrain G.Picture
renderBriefing = undefined

renderEditor :: Minibrain G.Picture
renderEditor = do
    
    perceptrons <- gets (nodes . editorData . sceneData)
    connections <- gets (edges . editorData . sceneData)
    selection <- gets (selectionRect . editorData . sceneData)
    let selectionPicture =
            case selection of
                Nothing -> G.Blank
                Just selRect -> renderSelection selRect
    return $ G.Color background
        --    $ renderBackground
           $ G.Pictures $ map renderPerceptron perceptrons
                       ++ map renderConnection connections
                       ++ [selectionPicture]
    

    where
    renderPerceptron :: Perceptron -> G.Picture
    renderPerceptron p =
        let w            = perceptronWidth
            h            = getPerceptronHeight p
            size         = SDL.V2 w h
            (SDL.V2 x y) = position p
            body = G.Color perceptronBodyColor $
                            fillRoundRectangle size perceptronBodyRoundness
            pins = map (renderPin p)
                       (zip [0..inputPinCount p - 1] (repeat InputPin) ++
                       zip [0..outputPinCount p - 1] (repeat OutputPin))
        in  G.Translate x y $ G.Pictures (reverse $ body : pins)
    renderPin :: Perceptron -> (Int, PinType) -> G.Picture
    renderPin perc (n, t) =
        let (SDL.V2 px py) = getPinRelativePosition perc n t
            size           = SDL.V2 pinWidth pinHeight
        in G.Translate px py $ G.Color pinColor $ fillRoundRectangle size 0
    renderConnection :: (Perceptron, Perceptron, Connection) -> G.Picture
    renderConnection (p1, p2, c) =
        let pos1 = getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
            pos2 = getPinAbsolutePosition p2 (dstPinNumber c) InputPin
        in G.Color pinColor $ thickLine pos1 pos2 connectionWidth
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

renderSimulation :: Minibrain G.Picture
renderSimulation = undefined