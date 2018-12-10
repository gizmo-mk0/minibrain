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

    perceptrons <- gets (perceptrons . editorData . sceneData)
    mapM_ renderPerceptron perceptrons

    SDL.present r
    where
    renderPerceptron :: Perceptron -> Minibrain ()
    renderPerceptron p@(Perceptron _ pos pins) = do
        (Config _ r) <- ask
        let w           = perceptronWidth
            h           = fromIntegral $ getPerceptronHeight p
            size        = SDL.V2 w h
            topLeft     = pos - (fmap (`div` 2) size)
            bottomRight = pos + (fmap (`div` 2) size)
        SDLP.fillRoundRectangle r topLeft bottomRight perceptronBodyRoundness
                                perceptronBodyColor
        mapM_ (renderPin p) pins
    renderPin :: Perceptron -> Pin -> Minibrain ()
    renderPin perc pin = do
        (Config _ r) <- ask
        let pinPos      = getPinPosition perc pin
            size        = SDL.V2 pinWidth pinHeight
            topLeft     = pinPos - (fmap (`div` 2) size)
            bottomRight = pinPos + (fmap (`div` 2) size)
        SDLP.fillRectangle r topLeft bottomRight pinColor

renderSimulation :: Minibrain ()
renderSimulation = undefined