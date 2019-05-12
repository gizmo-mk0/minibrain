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
import qualified NanoVG as NVG
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
import Globals
import Utils

data Config = Config
            { getWindow     :: SDL.Window
            , getWindowSize :: Vector2i
            -- , getRenderer   :: SDL.Renderer
            -- , getTexture    :: SDL.Texture }
            , getGlContext  :: SDL.GLContext
            , getNvgContext :: NVG.Context }

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
                    --  . (+ (L.V3 (-(fromIntegral w / 2)) (fromIntegral h / 2) 0))
                    --  . (* (L.V3 1 (-1) 1))
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
sceneNetwork cfg gd gdref (inp, frame) = mdo
    events      <- accumE (MouseMoveEvent (SDL.V2 0 0))
                        (unions [fmap const inputEvents, id <$ frameEvents])
    inputEvents <- fromAddHandler inp
    frameEvents <- fromAddHandler frame
    mousePosB   <- stepper (SDL.V2 0 0)
                $ fmap (\(cData, x) -> convertPoint cData x)
                $ (fmap (\cd' -> (,) cd') cameraDataB) <@> mouseMoveEvents
    let quitE = filterE (== KeyboardEvent SDL.KeycodeEscape SDL.Pressed) events
        cameraDataB = pure (cameraData gd)
        convertPoint cameraData p =
            toWorldCoords cameraData (getWindowSize cfg) p
        mouseMoveEvents :: Event Vector2f
        mouseMoveEvents = fmap (\(MouseMoveEvent x) -> x)
                        . filterE (\case MouseMoveEvent _ -> True;
                                            _ -> False)
                        $ events
    newGameData <- editorNetwork gd events mousePosB cameraDataB
    reactimate $ fmap (writeIORef gdref) newGameData

editorNetwork :: GameData -> Event InputEvent -> Behavior Vector2f
              -> Behavior CameraData
              -> MomentIO (Event GameData)
editorNetwork (GameData sd cd md) events mousePosB cameraDataB = mdo
    graphB <- stepper (graph $ editorData sd) graphE
    selectedNodesB <- accumB [] selectedNodesE
    lastClickB <- stepper (SDL.V2 0 0) leftPressAt
    lastRClickB <- stepper (SDL.V2 0 0) rightPressAt
    selectionRectB <- switchB (pure Nothing)
                              (unionWith const
                                         ((pure Nothing) <$ leftRelease)
                                         (selectionBoxB <$ emptyLeftPress))
    leftButtonDownB <- stepper False (unionWith const (True  <$ leftPress)
                                                      (False <$ leftRelease))
    -- nodeKnobUnderMouse :: Behaviour (Int, Float)
    nodeKnobUnderMouse <- stepper Nothing
                        $ fmap (\(g, mp) -> getNodeKnobAt mp g)
                        (((,) <$> graphB <*> mousePosB) <@ rightPress)
    connectionKnobUnderMouse <- stepper Nothing
                        $ fmap (\(g, mp) -> getConnectionKnobAt mp g)
                        (((,) <$> graphB <*> mousePosB) <@ rightPress)
    -- nodeUnderMouse :: Behavior (Maybe Bool) - Is there a node under the
    -- mouse, and if yes, is it among the selected ones
    nodeUnderMouse <- stepper Nothing
                   $ fmap (\(ed, mp) -> fmap (isNodeSelected ed . fst)
                                      $ getNodeAt mp (graph ed))
                          (((,) <$> editorDataB <*> mousePosB) <@ events)
    currentToolB <- accumB Nothing currentToolE
    selectedPinB <-
          stepper Nothing
        $ unionWith const
                    ( fmap (uncurry getPinAt)
                    $ ((,) <$> mousePosB <*> graphB) <@ leftPress)
                    ( Nothing <$ leftRelease )
    pinUnderMouse <- stepper False
                             ( fmap (\(g, mp) ->
                                        isJust $ (getPinAt mp g))
                             $ ((,) <$> graphB <*> mousePosB) <@ events)
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
        graphE = foldl1 (unionWith const)
                        [ moveNodesE
                        , connectE
                        , createNodeE
                        , deleteNodesE
                        , tuneKnobsE ]
        moveNodesE = fmap (\(ed, delta) -> moveSelectedNodes ed delta)
                          (((,) <$> editorDataB <*> dragB)
                          <@ (whenE (fmap (== Just Move) currentToolB) events))
        connectE = fmap (\(ed, mp) ->
                        connect ( (fromJust . selectedPin $ ed)
                                , (fromJust . getPinAt mp . graph $ ed))
                                (graph ed))
                 $ ((,) <$> editorDataB <*> mousePosB)
                   <@ (whenE (fmap (/= Nothing) selectedPinB)
                             (whenE pinUnderMouse leftRelease))
        createNodeE = fmap (uncurry addNodeAt)
                           ((,) <$> mousePosB <*> graphB) <@ emptyDClickE
        deleteNodesE =
            fmap (\(g, sn) -> deleteSelectedNodes (fmap fst sn) g)
                 (((,) <$> graphB <*> selectedNodesB) <@ pressedDelE)
        selectedNodesE =
            unions -- if left button is released, update the selected nodes' pos
                [ fmap (const . updateSelectedNodes)
                       (editorDataB <@ leftRelease)
                -- if the node under the mouse is not part os the selection,
                -- make this node the sole new selection
                , fmap (\(g, mp) -> const [fromJust $ getNodeAt mp g])
                       (((,) <$> graphB <*> mousePosB) <@
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
        tuneKnobsE = unionWith const tuneNodesE tuneConnectionsE
        tuneNodesE =
            fmap (\(g, Just (n, v), SDL.V2 mx my, SDL.V2 lx ly) ->
                    tunePerceptron n (clamp (-1) 1
                                        (v - ((my - ly) / tuneMouseDistance)))
                                   g)
            $ (((,,,) <$> graphB <*> nodeKnobUnderMouse <*> mousePosB
                      <*> lastRClickB)
                      <@ whenE (fmap (== Just Tune) currentToolB)
                            (whenE (fmap (/= Nothing) nodeKnobUnderMouse)
                                events))
        tuneConnectionsE =
            fmap (\(g, Just (n1, n2, v), SDL.V2 mx my, SDL.V2 lx ly) ->
                tuneConnection n1 n2 (clamp (-1) 1
                                        (v - ((my - ly) / tuneMouseDistance)))
                               g)
            $ (((,,,) <$> graphB <*> connectionKnobUnderMouse <*> mousePosB
                      <*> lastRClickB)
                      <@ whenE (fmap (== Just Tune) currentToolB)
                            (whenE (fmap (/= Nothing) connectionKnobUnderMouse)
                                events))
        currentToolE =
            unions [ (const Nothing)        <$ leftRelease
                   , (const Nothing)        <$ rightRelease
                   , (const (Just Connect)) <$ leftPressOnPin
                   , (const (Just Move))    <$ leftPressOnNode
                   , (const (Just Select))  <$ emptyLeftPress
                   , (const (Just Tune))    <$ rightPressOnNodeKnob
                   , (const (Just Tune))    <$ rightPressOnConnectionKnob]
        emptyDClickE :: Event Vector2f
        emptyDClickE = fmap snd
                     . filterE (\(g, mp) ->
                                       (getNodeAt mp g == Nothing)
                                    && (getPinAt  mp g == Nothing))
                     $ ((,) <$> graphB <*> mousePosB) <@ doubleClickE
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
        rightPress :: Event InputEvent
        rightPress =
            filterE (\case MouseClickEvent 1 SDL.ButtonRight SDL.Pressed -> True
                           _ -> False)
                    events
        rightPressAt :: Event Vector2f
        rightPressAt = mousePosB <@ rightPress
        rightPressOnNodeKnob :: Event (Int, Float)
        rightPressOnNodeKnob =
            filterJust
            . fmap (\(g, mp) -> getNodeKnobAt mp g)
            $ ((,) <$> graphB <*> mousePosB) <@ rightPress
        rightPressOnConnectionKnob :: Event (Int, Int, Float)
        rightPressOnConnectionKnob =
            filterJust
            . fmap (\(g, mp) -> getConnectionKnobAt mp g)
            $ ((,) <$> graphB <*> mousePosB) <@ rightPress
        rightRelease :: Event InputEvent
        rightRelease =
            filterE
                (\case MouseClickEvent 1 SDL.ButtonRight SDL.Released -> True
                       _ -> False) events
        leftRelease :: Event InputEvent
        leftRelease =
            filterE (\case MouseClickEvent 1 SDL.ButtonLeft SDL.Released -> True
                           _ -> False)
                    events
        leftPressOnNode :: Event Vector2f
        leftPressOnNode =
            fmap snd
            . filterE (\(g, mp) -> (isJust $ getNodeAt mp g))
            $ ((,) <$> graphB <*> mousePosB) <@ leftPress
        leftPressOnPin :: Event (Int, (PinType, Int, Vector2f))
        leftPressOnPin =
            filterJust
            . fmap (\(g, mp) -> (getPinAt mp g))
            $ ((,) <$> graphB <*> mousePosB) <@ leftPress
        emptyLeftPress :: Event Vector2f
        emptyLeftPress =
            fmap snd
            . filterE (\(g, mp) -> (getNodeAt mp g == Nothing)
                                 && (getPinAt  mp g == Nothing))
            $ ((,) <$> graphB <*> mousePosB) <@ leftPress
        pressedDelE :: Event InputEvent
        pressedDelE =
            filterE (== KeyboardEvent SDL.KeycodeDelete SDL.Pressed) events
    return newGameData

simulationNetwork :: GameData -> Event InputEvent -> Behavior Vector2f
                  -> Behavior CameraData
                  -> MomentIO (Event GameData)
simulationNetwork (GameData sd cd md) events mousePosB cameraDataB = undefined

-- titleNetwork = undefined
-- briefingNetwork = undefined
