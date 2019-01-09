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
    return $ G.Color background
        --    $ backgroundLines
           $ G.Pictures $ map renderPerceptron perceptrons

    -- connections <- gets (edges . editorData . sceneData)
    -- mapM_ renderConnection connections

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
    -- renderConnection :: (Perceptron, Perceptron, Connection) -> Minibrain ()
    -- renderConnection (p1, p2, c) = do
    --     r <- asks getRenderer
    --     let pos1 = getPinPosition p1 (srcPinNumber c) OutputPin
    --         pos2 = getPinPosition p2 (dstPinNumber c) InputPin
    --     -- thickLine :: MonadIO m => Renderer -> Pos -> Pos -> Width -> Color -> m () 
    --     SDLP.thickLine r pos1 pos2 connectionWidth pinColor
    -- -- TODO
    -- -- https://gamedev.stackexchange.com/questions/106438/zooming-in-sdl-2-0
    -- -- Maybe store the srcRect and dstRect in EditorData?
    -- renderBackground :: Minibrain ()
    -- renderBackground = undefined

renderSimulation :: Minibrain G.Picture
renderSimulation = undefined