{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- generic-lens stuff
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module GameData where
--
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..), asks)
import Control.Monad.State.Strict (StateT(..), MonadState(..), gets, modify)

import qualified Data.Map
import qualified SDL
import qualified Graphics.Gloss.Rendering as G
import qualified Linear as L
import Control.Lens ((^.), (.~), (&))
import Data.Generics.Product.Fields (field)

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
    let leftClick    = isMouseButtonJustPressed  inp SDL.ButtonLeft
        leftDown     = isMouseButtonDown         inp SDL.ButtonLeft
        leftReleased = isMouseButtonJustReleased inp SDL.ButtonLeft
    -- if left mouse button was just pressed:
    --   if there is anode under the mouse
    --      activate Move tool
    --      if the node is not selected
    --         delete all selection and make it the only selected node
    --   otherwise delete all selection
    -- update last mouse click position
    when leftClick $ do
        ed <- gets (editorData . sceneData)
        let newEditorData =
                (case getNodeAt mPos (graph ed) of
                    Just n  ->  
                        ed & field @"currentTool"   .~ Just Move
                           & field @"selectedNodes" .~
                                (if (fst n) `elem` (map fst (selectedNodes ed))
                                    then selectedNodes ed
                                    else [n])
                    Nothing ->  ed & field @"currentTool"   .~ Just Select
                                   & field @"selectedNodes" .~ [])
                & field @"mousePressedAt" .~ mPos
        modify (\gd@GameData{..} -> gd {sceneData = sceneData
                            {editorData = newEditorData}})
    -- if left mouse is down
    --    if move tool is active
    --        move all selected nodes
    --    if select tool is active
    --        update selection rectangle
    when leftDown $ do
        ed <- gets (editorData . sceneData)
        let newEditorData =
                (case currentTool ed of
                    Just Move   ->
                        moveSelectedNodes ed (mPos - mousePressedAt ed)
                    Just Select ->
                        ed & field @"selectionRect" .~
                            (Just (Rect2f (mousePressedAt ed)
                                  (mPos - mousePressedAt ed)))
                    _ -> ed) -- this shouldn't happen
        modify (\gd@GameData{..} -> gd {sceneData = sceneData
                            {editorData = newEditorData}})
    -- if left mouse button is just released
    --    if selection tool was active
    --        make nodes inside the selection rectangle the new selected nodes
    --    if move was active
    --        update selected nodes initial position
    -- delete selection rect
    -- deactivate current tool
    when leftReleased $ do
        ed <- gets (editorData . sceneData)
        let newEditorData =
                (case currentTool ed of
                    Just Select ->
                        ed & field @"selectedNodes" .~ collectSelectedNodes ed
                    Just Move   ->
                        ed & field @"selectedNodes" .~ updateSelectedNodes ed
                    _           -> ed)
                    & field @"selectionRect" .~ Nothing
                    & field @"currentTool"   .~ Nothing
        modify (\gd@GameData{..} -> gd {sceneData = sceneData
                            {editorData = newEditorData}})
    if isButtonDown inp SDL.KeycodeEscape
        then changeScene Quit
        else return ()
    
