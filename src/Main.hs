{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL

import Control.Monad (unless, when)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State.Strict (evalStateT, gets, modify)

import Scene
import GameData
import Input
import Globals
import Render

main :: IO ()
main = do
    -- Init SDL
    SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
    window   <- SDL.createWindow "Minibrain" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    -- Load resources
    let cfg = Config window renderer
        gameData = initData
    -- Run main loop
    runMinibrain cfg gameData mainLoop
    -- Free resources
    -- Uninitialize SDL
    SDL.destroyWindow window
    SDL.quit

initData :: GameData
initData = GameData (SceneData Editor TitleData BriefingData defaultEditorData SimulationData)
                    CameraData
                    defaultInputData

runMinibrain :: Config -> GameData -> Minibrain a -> IO a
runMinibrain config gameData (Minibrain m) =
    evalStateT (runReaderT m config) gameData

mainLoop :: Minibrain ()
mainLoop = do
    -- Collect input
    updateInput
    -- Scene logic
    advanceScene
    -- Render
    renderCurrentScene
    -- Decide next scene
    scene <- gets (currentScene . sceneData)
    let quit = scene == Quit
    unless quit mainLoop

updateInput :: Minibrain ()
updateInput = do
    events <- SDL.pollEvents
    mapM_ (\e -> modify (\gd@(GameData _ _ inpDat) ->
                    gd{inputData = modifyInput inpDat e})) events

advanceScene :: Minibrain ()
advanceScene = do
    inp <- gets inputData
    when (isButtonDown inp ButtonEsc) $ changeScene Quit

changeScene :: Scene -> Minibrain ()
changeScene s = modify (\gd@(GameData sd _ _) -> gd {sceneData = sd {currentScene = s}})

