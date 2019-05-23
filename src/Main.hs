{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified NanoVG    as NVG
import qualified Data.Set  as S
import qualified Graphics.GL.Core32 as GL

import Data.Time.Clock.System     ( SystemTime, getSystemTime, systemSeconds
                                  , systemNanoseconds )
import Reactive.Banana.Frameworks (compile, actuate)
import Control.Event.Handler      (Handler, newAddHandler)
import Data.IORef                 (IORef, newIORef, readIORef)
import Data.Maybe                 (listToMaybe)
import Data.Bits                  ((.|.))
import Foreign.C.Types            (CFloat(..), CInt(..))

import Scene.Editor        (mkEditor)
import Scene.Editor.Helper (mkGraph)
import Types               (Config(..))
import Input               (InputEvent, handleEvents)
import Render              (renderImage)
import Scene               (Scene(..), sceneNetwork, emptyScene)

import Utils
import Globals

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
    _ <- SDL.glCreateContext window
    _ <- glewInit
    putStrLn "Creating NanoVG context"
    nvgContext <- NVG.createGL3 (S.fromList [NVG.Antialias, NVG.StencilStrokes])
    _ <- NVG.createFont nvgContext "regular"
                        (NVG.FileName "dat/Roboto-Regular.ttf")
    let cfg = Config window (SDL.V2 c_width c_height) nvgContext

    putStrLn "Setting up input handler network"
    (inputHandler, inputFire) <- newAddHandler -- Handler InputEvent
    (frameHandler, frameFire) <- newAddHandler -- Handler ()
    sref <- newIORef initScene
    eNetwork <- compile (sceneNetwork initScene sref
                                      (inputHandler, frameHandler))
    actuate eNetwork
    putStrLn "Starting main loop"
    mainLoop cfg (inputFire, frameFire) sref
    -- Free resources
    -- Uninitialize SDL
    putStrLn "Cleaning up"
    SDL.destroyWindow window
    SDL.quit
    putStrLn "Finished"

loopIfNeeded :: Stack Scene -> IO () -> IO ()
loopIfNeeded (Stack _ []) _ = return ()
loopIfNeeded _ action = action

initScene :: Stack Scene
initScene = Utils.push (mkEditor (mkGraph ["foodDirection", "foodDistance"]
                                          ["rotate", "move"]))
                       $ Utils.init emptyScene 

mainLoop :: Config -> (Handler InputEvent, Handler ()) -> IORef (Stack Scene)
         -> IO ()
mainLoop cfg' fires' sref' = do
    t <- getSystemTime
    mainLoop' 60 t [] cfg' fires' sref'
    where
    mainLoop' :: Float -> SystemTime -> [InputEvent] -> Config
              -> (Handler InputEvent, Handler ()) -> IORef (Stack Scene)
              -> IO ()
    mainLoop' fps t es cfg (inputFire, frameFire) sref = do
        events <- fmap (es ++) handleEvents
        case listToMaybe events of
            Nothing -> frameFire () -- return ()
            Just e  -> inputFire e
        stack <- readIORef sref
        
        t' <- getSystemTime
        let nano = 1000 * 1000 * 1000
            diffSec = systemSeconds t' - systemSeconds t
            diffNanoSec =
                (systemNanoseconds t' - systemNanoseconds t) `mod` nano
        newT <- if ((diffSec > 1) ||
                   (diffNanoSec > round (fromIntegral nano / fps)))
                    then do
                        renderScene cfg $ top stack
                        return t'
                    else return t
        
        loopIfNeeded stack $
            mainLoop' fps newT (drop 1 events) cfg (inputFire, frameFire) sref

renderScene :: Config -> Scene -> IO ()
renderScene (Config window (SDL.V2 w h) c) scene = do
    let (NVG.Color (CFloat bg_r) (CFloat bg_g)
                   (CFloat bg_b) (CFloat bg_a)) = editorBackgroundColor
    GL.glClearColor bg_r bg_g bg_b bg_a
    GL.glClear (   GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT
                .|. GL.GL_STENCIL_BUFFER_BIT)
    NVG.beginFrame c (fromIntegral w) (fromIntegral h) 1

    renderImage c $ render scene

    NVG.endFrame c
    SDL.glSwapWindow window

    -- -- TODO make zoom relative to the screen center instead of the world center
    -- liftIO $ G.displayPicture (w, h) (G.makeColor 0 0 0 0) s zoom
    --        $ G.Translate dx dy
    --        $ G.Rotate rotation
    --        $ G.Scale zoom zoom
    --        $ sceneGeometry
    --        $ sd

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