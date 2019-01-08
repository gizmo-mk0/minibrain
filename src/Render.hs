module Render where

--
import qualified Data.IntMap as Map
import qualified SDL
import qualified SDL.Primitive as SDLP
import qualified Graphics.Gloss.Rendering as G

import Control.Monad.State.Strict (gets)
import Control.Monad.Reader (asks, liftIO)

import Types
import Globals
import GameData
import Scene

renderCurrentScene :: Minibrain ()
renderCurrentScene = do
    scene <- gets (currentScene . sceneData)
    w <- asks getWindow
    case scene of
        Title -> renderTitle
        Briefing -> renderBriefing
        Editor -> renderEditor
        Simulation -> renderSimulation
        Quit -> return ()
    SDL.glSwapWindow w
    

renderTitle :: Minibrain ()
renderTitle = undefined

renderBriefing :: Minibrain ()
renderBriefing = undefined

renderEditor :: Minibrain ()
renderEditor = do
    -- r <- asks getRenderer
    s <- asks getGlossState
    (SDL.V2 w h) <- asks getWindowSize
    -- SDL.rendererDrawColor r SDL.$= editorBackgroundColor
    -- SDL.clear r

    -- renderBackground
    -- test
    -- SDLP.thickLine r (SDL.V2 0 0) (SDL.V2 400 400) connectionWidth pinColor
    liftIO $ G.displayPicture (fromIntegral w, fromIntegral h) (G.makeColor 0 1 0 1) s 1 (G.Color (G.makeColor 1 0 0 1) (G.ThickCircle 5 2))
    -- liftIO $ G.renderPicture s 1 (G.Color (G.makeColor 1 1 1 1) (G.ThickCircle 50 20))
    -- endtest
    

    -- perceptrons <- gets (nodes . editorData . sceneData)
    -- mapM_ renderPerceptron perceptrons

    -- connections <- gets (edges . editorData . sceneData)
    -- mapM_ renderConnection connections

    -- SDL.present r
    -- where
    -- renderPerceptron :: Perceptron -> Minibrain ()
    -- renderPerceptron p = do
    --     r <- asks getRenderer
    --     let w           = perceptronWidth
    --         h           = fromIntegral $ getPerceptronHeight p
    --         size        = SDL.V2 w h
    --         topLeft     = position p - (fmap (`div` 2) size)
    --         bottomRight = position p + (fmap (`div` 2) size)
    --     SDLP.fillRoundRectangle r topLeft bottomRight perceptronBodyRoundness
    --                             perceptronBodyColor
    --     mapM_ (renderPin p)
    --           (zip [0..inputPinCount p - 1] (repeat InputPin) ++
    --            zip [0..outputPinCount p - 1] (repeat OutputPin))
    -- renderPin :: Perceptron -> (Int, PinType) -> Minibrain ()
    -- renderPin perc (n, t) = do
    --     r <- asks getRenderer
    --     let pinPos      = getPinPosition perc n t
    --         size        = SDL.V2 pinWidth pinHeight
    --         topLeft     = pinPos - (fmap (`div` 2) size)
    --         bottomRight = pinPos + (fmap (`div` 2) size)
    --     SDLP.fillRectangle r topLeft bottomRight pinColor
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

renderSimulation :: Minibrain ()
renderSimulation = undefined