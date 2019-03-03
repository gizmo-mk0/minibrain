{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- generic-lens
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
import Control.Lens ((^.), (.~), (&), set)
import Data.Generics.Product.Fields (field)
import Data.Maybe (maybeToList, isJust, fromJust)

import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Data.IORef

import GHC.Generics (Generic)

import Scene
import Input
import Types

data Config = Config
            { getWindow     :: SDL.Window
            , getWindowSize :: Vector2i
            , getGlossState :: G.State}

data GameData = GameData
              { sceneData     :: SceneData
              , cameraData    :: CameraData
              , mousePosBition :: Vector2f}
              deriving (Generic)

data CameraData = CameraData
                { cPosition   :: Vector2f
                , cRotation   :: Float
                , cZoom       :: Float }
                deriving (Show)

data TimeData = TimeData
              { currentTime    :: Float
              , sinceLastFrame :: Float }

-- cameradata windowSize position
toWorldCoords :: CameraData -> Vector2i -> Vector2f -> Vector2f
toWorldCoords (CameraData (SDL.V2 cx cy) r z) (SDL.V2 w h) (SDL.V2 x y) =
    let (L.V2 lx ly) = (^. L._xy)
                     -- applying camera transformation in reverse:
                     . L.rotate (L.axisAngle (L.V3 0 0 1) (pi * r / 180))
                     . (/ (L.V3 z z z))
                     . (+ (L.V3 (-cx) (-cy) 0))
                     -- convert from SDL screen coordinates:
                     . (+ (L.V3 (-(fromIntegral w / 2)) (fromIntegral h / 2) 0))
                     . (* (L.V3 1 (-1) 1))
                     $ (L.V3 x y 0)
    in  SDL.V2 lx ly

changeScene :: Scene -> GameData -> GameData
changeScene s gd = gd {sceneData = (sceneData gd) {currentScene = s}}

updateSelectionRect :: GameData -> Vector2f -> GameData
updateSelectionRect gd p =
    gd & field @"sceneData"
       . field @"editorData"
       . field @"selectionRect"
       .~ Just (Rect2f (SDL.V2 0 0) p)

sceneNetwork :: Config -> GameData -> IORef GameData
             -> (AddHandler InputEvent, AddHandler ()) -> MomentIO ()
sceneNetwork cfg (GameData sd cd md) gdref (inp, frame) = mdo
    inputEvents <- fromAddHandler inp
     -- We need empty events every frame to counteract `stepper`'s 1-frame delay
    frameEvents <- fromAddHandler frame
    events <- accumE (MouseMoveEvent (SDL.V2 0 0))
                        (unions [fmap const inputEvents, id <$ frameEvents])
    mousePosB <- stepper (SDL.V2 0 0)
              $ fmap (\(cData, x) -> convertPoint cData x)
              $ (fmap (\cd' -> (,) cd') cameraDataB) <@> mouseMoveEvents
    graphB <- stepper (graph $ editorData sd) graphE
    selectedNodesB <- accumB [] selectedNodesE
    lastClickB <- stepper (SDL.V2 0 0) leftPressAt
    selectionRectB <- switchB (pure Nothing)
                              (unionWith const
                                         ((pure Nothing) <$ leftRelease)
                                         (selectionBoxB <$ emptyLeftPress))
    leftButtonDownB <- stepper False (unionWith const (True <$ leftPress)
                                                      (False <$ leftRelease))
    -- nodeUnderMouse :: Behavior (Maybe Bool) - Is there a node under the
    -- mouse, and if yes, is it among the selected ones
    nodeUnderMouse <- stepper Nothing
                   $ fmap (\(ed, mp) -> fmap (isNodeSelected ed . fst)
                                      $ getNodeAt mp (graph ed))
                          (((,) <$> editorDataB <*> mousePosB) <@ events)
    currentToolB <- accumB Nothing currentToolE
    selectedPinB <- stepper Nothing
                  $ unionWith const
                              ( fmap (\(ed, mp) -> (getPinAt mp (graph ed)))
                              $ ((,) <$> editorDataB <*> mousePosB) <@ leftPress)
                              ( Nothing <$ leftRelease )
    pinUnderMouse <- stepper False
                             ( fmap (\(ed, mp) ->
                                        isJust $ (getPinAt mp (graph ed)))
                             $ ((,) <$> editorDataB <*> mousePosB) <@ events)
    let quitE = filterE (== KeyboardEvent SDL.KeycodeEscape SDL.Pressed) events
        newGameData =
            unionWith const
                ((changeScene Quit (GameData sd cd md)) <$ quitE)
                (gameDataB <@ events)
        dragB = pure (-) <*> mousePosB <*> lastClickB
        selectionBoxB = pure Just <*> (pure Rect2f <*> lastClickB <*> dragB)
        editorDataB = pure EditorData <*> graphB
                                      <*> selectionRectB
                                      <*> selectedNodesB
                                      <*> selectedPinB
        gameDataB = pure GameData <*> sceneDataB <*> cameraDataB <*> mousePosB
        sceneDataB = pure SceneData <*> pure (currentScene sd)
                                    <*> pure (titleData sd)
                                    <*> pure (briefingData sd)
                                    <*> editorDataB
                                    <*> pure (simulationData sd)
        cameraDataB = pure cd
        graphE = unionWith const (unionWith const moveNodesE connectE)
                                 createNodeE
        moveNodesE = fmap (\(ed, delta) -> moveSelectedNodes ed delta)
                          (((,) <$> editorDataB <*> dragB)
                          <@ (whenE (fmap (== Just Move) currentToolB) events))
        connectE = fmap (\(ed, mp) ->
                        connect (graph ed)
                                ( (fromJust . selectedPin $ ed)
                                , (fromJust . getPinAt mp . graph $ ed)))
                 $ ((,) <$> editorDataB <*> mousePosB)
                   <@ (whenE (fmap (/= Nothing) selectedPinB)
                             (whenE pinUnderMouse leftRelease))
        -- TODO for some reason, the created node is actually infinitely many
        -- TODO nodes. It works well for emptyLeftPress.
        createNodeE = fmap (\(ed, mpos) -> addNodeAt (graph ed) mpos)
                           ((,) <$> editorDataB <*> mousePosB) <@ doubleClickE
        selectedNodesE =
            unions -- if left button is released, update the selected nodes' pos
                [ fmap (const . updateSelectedNodes)
                       (editorDataB <@ leftRelease)
                -- if the node under the mouse is not part os the selection,
                -- make this node the sole new selection
                , fmap (\(ed, mp) -> const [fromJust $ getNodeAt mp (graph ed)])
                       (((,) <$> editorDataB <*> mousePosB) <@
                            (whenE (fmap ((== Just False))
                                         nodeUnderMouse) leftPress))
                -- If the player has clicked on an empty part, unselect
                -- every node
                , const [] <$ emptyLeftPress
                -- If a selection rect is active, selected nodes are the ones
                -- under the rect
                , fmap (const . collectSelectedNodes)
                        (editorDataB <@ (whenE (fmap (not . (== Nothing))
                                                     selectionRectB) events))]
        currentToolE =
            unions [ (const Nothing)        <$ leftRelease
                   , (const (Just Connect)) <$ leftPressOnPin
                   , (const (Just Move))    <$ leftPressOnNode
                   , (const (Just Select))  <$ emptyLeftPress ]
        doubleClickE :: Event InputEvent
        doubleClickE =
            filterE (\case MouseClickEvent 2 SDL.ButtonLeft SDL.Pressed -> True
                           _ -> False)
                    events
        leftPress :: Event InputEvent
        leftPress =
            filterE (\case MouseClickEvent 1 SDL.ButtonLeft SDL.Pressed -> True
                           _ -> False)
                    events
        leftPressAt :: Event Vector2f
        leftPressAt = mousePosB <@ leftPress
        leftRelease :: Event InputEvent
        leftRelease =
            filterE (\case MouseClickEvent 1 SDL.ButtonLeft SDL.Released -> True
                           _ -> False)
                    events
        mouseMoveEvents :: Event Vector2f
        mouseMoveEvents = fmap (\(MouseMoveEvent x) -> x)
                        . filterE (\case MouseMoveEvent _ -> True;
                                            _ -> False)
                        $ events
        convertPoint cameraData x =
            toWorldCoords cameraData (getWindowSize cfg) x
        leftPressOnNode :: Event Vector2f
        leftPressOnNode =
            fmap snd
            . filterE (\(ed, mp) -> (isJust $ getNodeAt mp (graph ed)))
            $ ((,) <$> editorDataB <*> mousePosB) <@ leftPress
        leftPressOnPin :: Event (Int, (PinType, Int, Vector2f))
        leftPressOnPin =
            filterJust
            . fmap (\(ed, mp) -> (getPinAt mp (graph ed)))
            $ ((,) <$> editorDataB <*> mousePosB) <@ leftPress
        emptyLeftPress :: Event Vector2f
        emptyLeftPress =
            fmap snd
            . filterE (\(ed, mp) -> (getNodeAt mp (graph ed) == Nothing)
                                 && (getPinAt  mp (graph ed) == Nothing))
            $ ((,) <$> editorDataB <*> mousePosB) <@ leftPress
    reactimate $ fmap (writeIORef gdref) newGameData

-- titleNetwork = undefined
-- simulationNetwork = undefined
-- briefingNetwork = undefined

