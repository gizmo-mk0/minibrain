module Render where

--
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

    perceptrons <- gets (perceptrons . editorData . sceneData)
    mapM_ renderPerceptron perceptrons

    SDL.clear r
    SDL.present r
    where
    renderPerceptron :: Perceptron -> Minibrain ()
    renderPerceptron p@(Perceptron pos pins) = do
        (Config _ r) <- ask
        let w = perceptronWidth
            h = fromIntegral $ getPerceptronHeight p
        SDLP.fillRoundRectangle r pos (SDL.V2 w h) perceptronBodyRoundness
                                perceptronBodyColor

renderSimulation :: Minibrain ()
renderSimulation = undefined