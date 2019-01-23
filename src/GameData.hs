{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module GameData where
--
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..), asks)
import Control.Monad.State.Strict (StateT(..), MonadState(..), gets, modify)

import qualified Data.Map
import qualified SDL
import qualified Graphics.Gloss.Rendering as G
import qualified Linear as L
import Control.Lens ((^.))

import Scene
import Input
import Types

newtype Minibrain a = Minibrain (ReaderT Config (StateT GameData IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,
              MonadState GameData)

data Config = Config
            { getWindow     :: SDL.Window
            , getWindowSize :: Vector2i
            , getGlossState :: G.State }

data GameData = GameData
              { sceneData  :: SceneData
              , cameraData :: CameraData
              , inputData  :: InputData }

data CameraData = CameraData
                { cPosition   :: Vector2f
                , cRotation   :: Float
                , cZoom       :: Float }
                deriving (Show)

toWorldCoords :: Vector2f -> Minibrain Vector2f
toWorldCoords (SDL.V2 x y) = do
    (SDL.V2 w h)                    <- asks getWindowSize
    (CameraData (SDL.V2 cx cy) r z) <- gets cameraData
    let (L.V2 lx ly) = (^. L._xy)
                     -- applying camera transformation in reverse:
                     . L.rotate (L.axisAngle (L.V3 0 0 1) (pi * r / 180))
                     . (/ (L.V3 z z z))
                     . (+ (L.V3 (-cx) (-cy) 0))
                     -- convert from SDL screen coordinates:
                     . (+ (L.V3 (-(fromIntegral w / 2)) (fromIntegral h / 2) 0))
                     . (* (L.V3 1 (-1) 1))
                     $ (L.V3 x y 0)
    return $ SDL.V2 lx ly

changeScene :: Scene -> Minibrain ()
changeScene s = modify (\gd@GameData{..} ->
                                gd {sceneData = sceneData {currentScene = s}})

advanceScene :: Minibrain ()
advanceScene = do
    cd <- gets sceneData
    case currentScene cd of
        Title       -> return () -- TODO
        Briefing    -> return () -- TODO
        Editor      -> advanceEditor
        Simulation  -> return () -- TODO
        Quit        -> return ()

advanceEditor :: Minibrain ()
advanceEditor = do
    -- get input
    inp <- gets inputData
    -- Convert mouse coordinate into world coordinates
    mPos <- toWorldCoords (fmap fromIntegral . mousePosition . mouse $ inp)
    -- If mouse left is down, and there is no selected node, we are in selection
    -- mode
     -- check if mouse is clicked
    if isMouseButtonJustPressed inp SDL.ButtonLeft
        then do
            let newEditorData gd =
                    (editorData (sceneData gd)) {mousePressedAt = mPos}
            modify (\gd@GameData{..} -> gd {sceneData = sceneData
                                              {editorData = newEditorData gd}})
        else return ()
    if isButtonDown inp SDL.KeycodeEscape
        then changeScene Quit
        else return ()
    ed <- gets (editorData . sceneData)
    let selrect =
            if (isMouseButtonDown inp SDL.ButtonLeft) &&
                (null (selectedNodes ed))
                then Just $ Rect2f (mousePressedAt ed) (mPos - (mousePressedAt ed))
                else Nothing
    modify (\gd@GameData{..} -> gd {sceneData = sceneData
                {editorData = (editorData sceneData) {selectionRect = selrect}}})
       
