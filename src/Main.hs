{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified NanoVG    as NVG
import qualified Data.Set  as S
import qualified Graphics.GL.Core32 as GL

import Data.Time.Clock.System ( SystemTime, getSystemTime, systemSeconds
                              , systemNanoseconds )
import Control.Varying.Core   ( runVarT, Var )
import Control.Varying.Event  ( Event, event, noevent )
import Data.Functor.Identity  ( runIdentity )
import Control.Event.Handler  ( Handler, newAddHandler )
import Data.IORef             ( IORef, newIORef, readIORef )
import Data.Maybe             ( listToMaybe )
import Data.Bits              ( (.|.) )
import Foreign.C.Types        ( CFloat(..), CInt(..) )

import Scene.Editor         ( mkEditor )
import Scene.Simulation     ( mkSimulation )
import Scene.Editor.Helper  ( mkGraph )
import Scene.Editor.Globals ( editorBackgroundColor )
import Types                ( Config(..) )
import Input                ( InputEvent, handleEvents )
import Render               ( renderImage )
import Scene                ( Scene(..), emptyScene, updateStack
                            , StackCommand(..), isNone )

import Utils

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

    putStrLn "Starting main loop"
    mainLoop cfg initScene
    -- Free resources
    -- Uninitialize SDL
    putStrLn "Cleaning up"
    SDL.destroyWindow window
    SDL.quit
    putStrLn "Finished"

initScene :: Stack (Var (Event InputEvent) Scene)
initScene = Utils.push (mkEditor g)
          $ Utils.init emptyScene 
    where
    g = (mkGraph ["foodDirection", "foodDistance"] ["rotate", "move"])

-- TODO process more than one event per frame
mainLoop :: Config -> Stack (Var (Event InputEvent) Scene) -> IO ()
mainLoop cfg' stack = do
    t <- getSystemTime
    mainLoop' 60 t cfg' stack
    where
    mainLoop' :: Float -> SystemTime -> Config
              -> Stack (Var (Event InputEvent) Scene)-> IO ()
    mainLoop' fps t cfg stack = do
        events <- handleEvents
        
        t' <- getSystemTime
        let nano = 1000 * 1000 * 1000
            diffSec = systemSeconds t' - systemSeconds t
            diffNanoSec =
                (systemNanoseconds t' - systemNanoseconds t) `mod` nano
            (newScene, newStack) = stepStack events stack
        newT <- if ((diffSec > 1) ||
                   (diffNanoSec > round (fromIntegral nano / fps)))
                    then do
                        renderScene cfg newScene
                        return t'
                    else return t
        
        case newStack of
            (Stack _ []) -> return ()
            _            -> mainLoop' fps newT cfg newStack

stepStack :: [InputEvent] -> Stack (Var (Event InputEvent) Scene)
          -> (Scene, Stack (Var (Event InputEvent) Scene))
stepStack events s =
    case events of
        []     -> stepStack' noevent s
        (e:[]) -> stepStack' (event e) s
        (e:es) -> stepStack es $ snd $ stepStack' (event e) s
    where
    stepStack' :: Event InputEvent -> Stack (Var (Event InputEvent) Scene)
               -> (Scene, Stack (Var (Event InputEvent) Scene))
    stepStack' e s =
        let (scene, newSceneArr) = runIdentity $ runVarT (top s) e
        in  ( scene
            , if isNone (cmd scene)
                then updateStack (Replace newSceneArr) s
                else updateStack (cmd scene) s )

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