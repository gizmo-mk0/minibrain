module Render where

--
import qualified Data.IntMap as Map
import qualified SDL
import qualified SDL.Primitive as SDLP

import Control.Monad.State.Strict (gets)
import Control.Monad.Reader (ask)

import Types
import Globals
import GameData
import Scene

renderCurrentScene :: Minibrain ()
renderCurrentScene = do
    scene <- gets (currentScene . sceneData)
    case scene of
        Title -> renderTitle
        Briefing -> renderBriefing
        Editor -> renderEditor
        Simulation -> renderSimulation
        Quit -> return ()

renderTitle :: Minibrain ()
renderTitle = undefined

renderBriefing :: Minibrain ()
renderBriefing = undefined

renderEditor :: Minibrain ()
renderEditor = do
    (Config _ r) <- ask
    SDL.rendererDrawColor r SDL.$= editorBackgroundColor
    SDL.clear r

    perceptrons <- gets (nodes . editorData . sceneData)
    mapM_ renderPerceptron perceptrons

    -- connections <- gets (edges . editorData . sceneData)
    -- mapM_ renderConnection connections

    SDL.present r
    where
    renderPerceptron :: Perceptron -> Minibrain ()
    renderPerceptron p = do
        (Config _ r) <- ask
        let w           = perceptronWidth
            h           = fromIntegral $ getPerceptronHeight p
            size        = SDL.V2 w h
            topLeft     = position p - (fmap (`div` 2) size)
            bottomRight = position p + (fmap (`div` 2) size)
        SDLP.fillRoundRectangle r topLeft bottomRight perceptronBodyRoundness
                                perceptronBodyColor
        mapM_ (renderPin p)
              (zip [0..inputPinCount p - 1] (repeat InputPin) ++
               zip [0..outputPinCount p - 1] (repeat OutputPin))
    renderPin :: Perceptron -> (Int, PinType) -> Minibrain ()
    renderPin perc (n, t) = do
        (Config _ r) <- ask
        let pinPos      = getPinPosition perc n t
            size        = SDL.V2 pinWidth pinHeight
            topLeft     = pinPos - (fmap (`div` 2) size)
            bottomRight = pinPos + (fmap (`div` 2) size)
        SDLP.fillRectangle r topLeft bottomRight pinColor

renderSimulation :: Minibrain ()
renderSimulation = undefined