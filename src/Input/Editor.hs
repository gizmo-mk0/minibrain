{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Input.Editor where

import qualified SDL

import Reactive.Banana
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import Data.Maybe (maybeToList, isJust, fromJust)

import GameData
import Scene
import Types
import Globals
import Utils
import Scene.Editor

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
    rightButtonDownB <- stepper False (unionWith const (True  <$ rightPress)
                                                (False <$ rightRelease))
    -- nodeKnobUnderMouse :: Behaviour (Int, Float)
    nodeKnobUnderMouseR <- stepper Nothing
                        $ fmap (\(g, mp) -> getNodeKnobAt mp g)
                        (((,) <$> graphB <*> mousePosB) <@ rightPress)
    connectionKnobUnderMouseR <- stepper Nothing
                               $ fmap (\(g, mp) -> getConnectionKnobAt mp g)
                               (((,) <$> graphB <*> mousePosB) <@ rightPress)
    connectionKnobUnderMouseL <- stepper Nothing
                              $ fmap (\(g, mp) -> getConnectionKnobAt mp g)
                              (((,) <$> graphB <*> mousePosB) <@ leftPress)
    -- nodeUnderMouse :: Behavior (Maybe Bool) - Is there a node under the
    -- mouse, and if yes, is it among the selected ones
    nodeUnderMouse <- stepper Nothing
            $ fmap (\(ed, mp) -> fmap (isNodeSelected ed . fst)
                                $ getNodeAt mp (graph ed))
                    (((,) <$> editorDataB <*> mousePosB) <@ events)
    currentToolB <- accumB Nothing currentToolE
    selectedPinB <- stepper Nothing $
                        unionWith const
                                  ( fmap (uncurry getPinAt)
                                  $ ((,) <$> mousePosB <*> graphB) <@ leftPress)
                                  ( Nothing <$ leftRelease )
    pinUnderMouse <- stepper False
                             ( fmap (\(g, mp) ->
                                         isJust $ (getPinAt mp g))
                             $ ((,) <$> graphB <*> mousePosB) <@ events)
    currentSceneB <- stepper Editor (Simulation <$ pressedSpaceE)
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
        sceneDataB = pure SceneData <*> currentSceneB -- pure (currentScene sd)
                                    <*> pure (titleData sd)
                                    <*> pure (briefingData sd)
                                    <*> editorDataB
                                    <*> pure (simulationData sd)
        graphE = foldl1 (unionWith const)
                    [ moveNodesE
                    , connectE
                    , createNodeE
                    , deleteNodesE
                    , tuneKnobsE
                    , deleteConnectionE ]
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
        deleteConnectionE =
            fmap (\(g, Just (n1, n2, _)) -> deleteConnection n1 n2 g)
            $ (((,) <$> graphB <*> connectionKnobUnderMouseL)
                <@ (whenE (fmap (/= Nothing) connectionKnobUnderMouseL)
                            events))
        tuneKnobsE = unionWith const tuneNodesE tuneConnectionsE
        tuneNodesE =
            fmap (\(g, Just (n, v), SDL.V2 mx my, SDL.V2 lx ly) ->
                    tunePerceptron n (clamp (-1) 1
                                        (v - ((my - ly) / tuneMouseDistance)))
                                    g)
            $ (((,,,) <$> graphB <*> nodeKnobUnderMouseR <*> mousePosB
                        <*> lastRClickB)
                        <@ whenE (fmap (== Just Tune) currentToolB)
                            (whenE (fmap (/= Nothing) nodeKnobUnderMouseR)
                                events))
        tuneConnectionsE =
            fmap (\(g, Just (n1, n2, v), SDL.V2 mx my, SDL.V2 lx ly) ->
                tuneConnection n1 n2 (clamp (-1) 1
                                        (v - ((my - ly) / tuneMouseDistance)))
                                g)
            $ (((,,,) <$> graphB <*> connectionKnobUnderMouseR <*> mousePosB
                        <*> lastRClickB)
                        <@ whenE (fmap (== Just Tune) currentToolB)
                            (whenE (fmap (/= Nothing) connectionKnobUnderMouseR)
                                events))
        currentToolE =
            unions [ (const Nothing)        <$ leftRelease
                    , (const Nothing)        <$ rightRelease
                    , (const (Just Connect)) <$ leftPressOnPin
                    , (const (Just Move))    <$ leftPressOnNode
                    , (const (Just Select))  <$ emptyLeftPress
                    , (const (Just Tune))    <$ rightPressOnNodeKnob
                    , (const (Just Tune))    <$ rightPressOnConnectionKnob ]
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
        leftAndRightPress :: Event InputEvent
        leftAndRightPress =
            unionWith const (whenE leftButtonDownB rightPress)
                            (whenE rightButtonDownB leftPress)
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
        pressedSpaceE :: Event InputEvent
        pressedSpaceE =
            filterE (== KeyboardEvent SDL.KeycodeSpace SDL.Pressed) events
    return newGameData

