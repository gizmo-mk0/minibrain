{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
-- import qualified SDL.Video as SDL
import qualified Graphics.Gloss.Rendering as G

import Control.Monad (unless, when)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State.Strict (evalStateT, gets, modify)

import Scene
import GameData
import Input
import Globals
import Render


width = 800 :: Float
c_width = fromIntegral $ floor width
height = 600 :: Float
c_height = fromIntegral $ floor height

main :: IO ()
main = do
    -- Init SDL
    SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
    window   <- SDL.createWindow "Minibrain" (SDL.defaultWindow
                    { SDL.windowInitialSize = SDL.V2 (fromIntegral $ floor width)
                                                    (fromIntegral $ floor height)
                    , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL })
    -- renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    context <- SDL.glCreateContext window
    glossState <- G.initState

    -- Load resources
    let cfg = Config window (SDL.V2 c_width c_height) glossState -- texture
        gameData = initData
    -- Run main loop
    runMinibrain cfg gameData mainLoop
    -- Free resources
    -- Uninitialize SDL
    SDL.destroyWindow window
    SDL.quit

initData :: GameData
initData = GameData (SceneData Editor TitleData BriefingData defaultEditorData SimulationData)
                    (CameraData (SDL.V2 0 0) 0 1)
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
    -- Update the camera
    updateCamera
    -- Render
    renderCurrentScene
    -- Decide next scene
    scene <- gets (currentScene . sceneData)
    let quit = scene == Quit
    unless quit mainLoop

updateInput :: Minibrain ()
updateInput = do
    events <- SDL.pollEvents
    modify (\gd@(GameData _ _ inpDat) ->
        gd{inputData = foldr (flip modifyInput) inpDat events})

advanceScene :: Minibrain ()
advanceScene = do
    inp <- gets inputData
    when (isButtonDown inp SDL.KeycodeEscape) $ changeScene Quit

changeScene :: Scene -> Minibrain ()
changeScene s = modify (\gd@(GameData sd _ _) -> gd {sceneData = sd {currentScene = s}})

updateCamera :: Minibrain ()
updateCamera = do
    inp <- gets inputData
    when (isButtonDown inp SDL.KeycodeLeft)  $ modify (\gd -> gd {cameraData = rotateCamera (cameraData gd) 0.5})
    when (isButtonDown inp SDL.KeycodeRight) $ modify (\gd -> gd {cameraData = rotateCamera (cameraData gd) (-0.5)})
    where
    rotateCamera c r = c {cRotation = cRotation c + r}