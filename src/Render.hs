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
    renderPerceptron p@(Perceptron pos pins) = do
        (Config _ r) <- ask
        let w = perceptronWidth
            h = fromIntegral $ getPerceptronHeight p
            size = SDL.V2 w h
            topLeft = pos - (fmap (`div` 2) size)
            bottomRight = pos + (fmap (`div` 2) size)
        SDLP.fillRoundRectangle r topLeft bottomRight perceptronBodyRoundness
                                perceptronBodyColor
        mapM_ (renderPin p) (zip [0..] $ Map.elems (Map.filter (\(Pin pType _) -> pType == InputPin) pins))
        mapM_ (renderPin p) (zip [0..] $ Map.elems (Map.filter (\(Pin pType _) -> pType == OutputPin) pins))
    renderPin :: Perceptron -> (Int, Pin) -> Minibrain ()
    renderPin perc@(Perceptron pos@(SDL.V2 px py) _) (ix, pin@(Pin pinType _)) = do
        (Config _ r) <- ask
        let verticalPos = -(parentHeight `div` 2)
                        + perceptronBodyRoundness
                        + (fromIntegral ix * perceptronModuleHeight)
                        + (perceptronModuleHeight `div` 2)
            parentHeight = getPerceptronHeight perc
            pinPos = case pinType of
                        InputPin  -> pos - (SDL.V2 (perceptronWidth `div` 2) verticalPos)
                        OutputPin -> pos + (SDL.V2 (perceptronWidth `div` 2) verticalPos)
            size = SDL.V2 pinWidth pinHeight
            topLeft = pinPos - (fmap (`div` 2) size)
            bottomRight = pinPos + (fmap (`div` 2) size)
        SDLP.fillRectangle r topLeft bottomRight pinColor

renderSimulation :: Minibrain ()
renderSimulation = undefined