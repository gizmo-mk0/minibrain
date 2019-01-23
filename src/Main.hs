{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified SDL
-- import qualified SDL.Video as SDL
import qualified Graphics.Gloss.Rendering as G

import Control.Monad (unless, when)
import Control.Monad.Reader (runReaderT, ask, liftIO)
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
    window <- SDL.createWindow "Minibrain" (SDL.defaultWindow
                { SDL.windowInitialSize =
                    fmap fromIntegral $ SDL.V2 (floor width) (floor height)
                , SDL.windowGraphicsContext =
                    SDL.OpenGLContext SDL.defaultOpenGL })
    context <- SDL.glCreateContext window
    glossState <- G.initState

    -- Load resources
    let cfg = Config window (SDL.V2 c_width c_height) glossState
        gameData = initData
    -- Run main loop
    runMinibrain cfg gameData mainLoop
    -- Free resources
    -- Uninitialize SDL
    SDL.destroyWindow window
    SDL.quit

initData :: GameData
initData = GameData (SceneData Editor TitleData BriefingData defaultEditorData
                               SimulationData)
                    (CameraData (SDL.V2 0 0) 0 1)
                    defaultInputData

runMinibrain :: Config -> GameData -> Minibrain a -> IO a
runMinibrain config gameData (Minibrain m) =
    evalStateT (runReaderT m config) gameData

mainLoop :: Minibrain ()
mainLoop = do
    -- Collect and handle events (update input, etc.)
    handleEvents
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
    where

handleEvents :: Minibrain ()
handleEvents = do
    events <- SDL.pollEvents
    modify (\gd@GameData{..} ->
        gd {inputData = foldl modifyInput (advanceInputData inputData) events})
    
    -- inpDat <- gets inputData
    -- liftIO $ appendFile "log.txt" $ "[EVENTS] \n" ++ show events ++ "\n[INPUT DATA]\n" ++ show inpDat ++ "\n[-------]"

updateCamera :: Minibrain ()
updateCamera = do
    inp <- gets inputData
    when (isButtonDown inp SDL.KeycodeLeft)  $ modifyCamera (rotateCamera   1)
    when (isButtonDown inp SDL.KeycodeRight) $ modifyCamera (rotateCamera (-1))
    when (isButtonDown inp SDL.KeycodeUp)    $ modifyCamera (zoomCamera   1.1)
    when (isButtonDown inp SDL.KeycodeDown)  $ modifyCamera (zoomCamera   0.9)
    when (isButtonDown inp SDL.KeycodeW)     $ modifyCamera (moveCamera   (SDL.V2    0 (-10)))
    when (isButtonDown inp SDL.KeycodeS)     $ modifyCamera (moveCamera   (SDL.V2    0   10))
    when (isButtonDown inp SDL.KeycodeA)     $ modifyCamera (moveCamera   (SDL.V2   10    0))
    when (isButtonDown inp SDL.KeycodeD)     $ modifyCamera (moveCamera   (SDL.V2 (-10)   0))
    where
    modifyCamera :: (CameraData -> CameraData) -> Minibrain ()
    modifyCamera f = modify $ \gd -> gd {cameraData = f (cameraData gd)}
    rotateCamera r c = c {cRotation = cRotation c + r}
    zoomCamera   z c = c {cZoom     = cZoom c     * z}
    moveCamera   v c = c {cPosition = cPosition c + v}