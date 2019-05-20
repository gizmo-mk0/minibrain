{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified Data.Map  as Map
import qualified NanoVG    as NVG
import qualified Data.Set  as S

import Data.Maybe (catMaybes, listToMaybe)
import Data.Time.Clock.System

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Data.IORef

import Scene
import GameData
import Input
import Render
import Types

import Foreign.C.Types
foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

width = 1600 :: Float
c_width = fromIntegral $ floor width
height = 1000 :: Float
c_height = fromIntegral $ floor height

main :: IO ()
main = do
    -- Init SDL
    putStrLn "Initializing SDL"
    SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
    let openGLContext = SDL.defaultOpenGL
            { SDL.glProfile = SDL.Core SDL.Normal 3 2 }
    window <- SDL.createWindow "Minibrain" (SDL.defaultWindow
                { SDL.windowInitialSize =
                    (fromIntegral . floor) <$> SDL.V2 width height
                , SDL.windowGraphicsContext =
                    SDL.OpenGLContext openGLContext })
    putStrLn "Creating OpenGL context"
    glContext <- SDL.glCreateContext window
    _ <- glewInit
    putStrLn "Creating NanoVG context"
    nvgContext <- NVG.createGL3 (S.fromList [NVG.Antialias, NVG.StencilStrokes])
    NVG.createFont nvgContext "regular"
                   (NVG.FileName "dat/Roboto-Regular.ttf")
    let cfg = Config window (SDL.V2 c_width c_height) glContext nvgContext

    putStrLn "Setting up input handler network"
    (inputHandler, inputFire) <- newAddHandler -- Handler InputEvent
    (frameHandler, frameFire) <- newAddHandler -- Handler ()
    gdref <- newIORef initData
    eNetwork <- compile (sceneNetwork cfg initData gdref
                                      (inputHandler, frameHandler))
    actuate eNetwork
    putStrLn "Starting main loop"
    mainLoop cfg (inputFire, frameFire) gdref
    -- Free resources
    -- Uninitialize SDL
    putStrLn "Cleaning up"
    SDL.destroyWindow window
    SDL.quit
    putStrLn "Finished"

loopIfNeeded :: GameData -> IO () -> IO ()
loopIfNeeded gd action =
    if (currentScene . sceneData) gd == Quit then return () else action

initData :: GameData
initData = GameData (SceneData Editor TitleData BriefingData defaultEditorData
                               SimulationData)
                    (CameraData (SDL.V2 0 0) 0 1)
                    (SDL.V2 0 0)

mainLoop :: Config -> (Handler InputEvent, Handler ()) -> IORef GameData
         -> IO ()
mainLoop cfg fires gdref = do
    t <- getSystemTime
    mainLoop' 60 t [] cfg fires gdref
    where
    mainLoop' fps t es cfg (inputFire, frameFire) gdref = do
        events <- fmap (es ++) handleEvents
        case listToMaybe events of
            Nothing -> frameFire () -- return ()
            Just e  -> inputFire e
        gd <- readIORef gdref
        
        t' <- getSystemTime
        let nano = 1000 * 1000 * 1000
            diffSec = systemSeconds t' - systemSeconds t
            diffNanoSec =
                (systemNanoseconds t' - systemNanoseconds t) `mod` nano
        newT <- if ((diffSec > 1) ||
                   (diffNanoSec > round (fromIntegral nano / fps)))
                    then do
                        renderCurrentScene cfg gd
                        return t'
                    else return t
        
        loopIfNeeded gd $
            mainLoop' fps newT (drop 1 events) cfg (inputFire, frameFire) gdref

inputEvent :: SDL.Event -> Maybe InputEvent
inputEvent e =
    case SDL.eventPayload e of
        SDL.KeyboardEvent kbe -> Just $
            KeyboardEvent (SDL.keysymKeycode (SDL.keyboardEventKeysym kbe))
                        (SDL.keyboardEventKeyMotion kbe)
        SDL.MouseMotionEvent mme -> Just $
            let (SDL.P mpos) = SDL.mouseMotionEventPos mme
            in  MouseMoveEvent (fmap fromIntegral mpos)
        SDL.MouseButtonEvent mbe ->
            Just $
                MouseClickEvent (fromIntegral $ SDL.mouseButtonEventClicks mbe)
                                (SDL.mouseButtonEventButton mbe)
                                (SDL.mouseButtonEventMotion mbe)
        SDL.MouseWheelEvent mwe -> Just $
            let (SDL.V2 _ amount) = SDL.mouseWheelEventPos mwe
            in  case SDL.mouseWheelEventDirection mwe of
                SDL.ScrollNormal  -> MouseWheelEvent $ fromIntegral amount
                SDL.ScrollFlipped -> MouseWheelEvent $ fromIntegral (-amount)
        _ -> Nothing

handleEvents :: IO [InputEvent]
handleEvents = fmap (catMaybes . map inputEvent) SDL.pollEvents

-- updateCamera :: Minibrain ()
-- updateCamera = do
--     inp <- gets inputData
--     when (isButtonDown inp SDL.KeycodeLeft)  $ modifyCamera (rotateCamera   1)
--     when (isButtonDown inp SDL.KeycodeRight) $ modifyCamera (rotateCamera (-1))
--     when (isButtonDown inp SDL.KeycodeUp)    $ modifyCamera (zoomCamera   1.1)
--     when (isButtonDown inp SDL.KeycodeDown)  $ modifyCamera (zoomCamera   0.9)
--     when (isButtonDown inp SDL.KeycodeW)     $ modifyCamera (moveCamera   (SDL.V2    0 (-10)))
--     when (isButtonDown inp SDL.KeycodeS)     $ modifyCamera (moveCamera   (SDL.V2    0   10))
--     when (isButtonDown inp SDL.KeycodeA)     $ modifyCamera (moveCamera   (SDL.V2   10    0))
--     when (isButtonDown inp SDL.KeycodeD)     $ modifyCamera (moveCamera   (SDL.V2 (-10)   0))
--     where
--     modifyCamera :: (CameraData -> CameraData) -> Minibrain ()
--     modifyCamera f = modify $ \gd -> gd {cameraData = f (cameraData gd)}
--     rotateCamera r c = c {cRotation = cRotation c + r}
--     zoomCamera   z c = c {cZoom     = cZoom c     * z}
--     moveCamera   v c = c {cPosition = cPosition c + v}