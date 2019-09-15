{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Scene.Editor.Input where

import qualified SDL

import Control.Varying.Core  ( Var, arr, (>>>), mkState, VarT(..), runVarT
                             , var, delay )
import Control.Varying.Event ( Event, startWith, onTrue, onWhen, use, anyE
                             , foldStream, onUnique, filterE, event, never )
import Control.Arrow         ( (&&&), first, second )
import Data.Maybe            ( isJust, fromJust )

import Scene    ( StackCommand(..), Scene(..) )
import Types    ( Vector2f, Rect2f(..) )
import Utils    ( clamp )
import Input    ( InputEvent(..) )

import Scene.Editor.Helper
import Scene.Editor.Globals
import Scene.Editor.Render ( renderEditor )

import Scene.Simulation ( mkSimulation )

-- Fixed varying's version, so that the first Var doesn't need to have the same
-- type for its input and output
bothE :: Monad m
      => (b -> c -> d) -> VarT m a (Event b) -> VarT m a (Event c)
      -> VarT m a (Event d)
bothE f va vb = (\ea eb -> f <$> ea <*> eb) <$> va <*> vb

-- A version which works on tuples
bothE' :: Monad m
       => (b -> c -> d) -> VarT m a (Event b, Event c) -> VarT m a (Event d)
bothE' f v = (\(ea, eb) -> f <$> ea <*> eb) <$> v

-- A version of onlyWhen which updates the inactive Var in each step
onlyWhen' :: Var a b -- ^ @v@ - The value stream
          -> (a -> Bool) -- ^ @f@ - The predicate to run on @v@'s input values.
          -> Var a (Event b)
onlyWhen' v f = v `onlyWhenE'` hot
    where hot = var id >>> onWhen f

-- A version of onlyWhenE which updates the inactive Var in each step
onlyWhenE' :: Var a b -> Var a (Event c) -> Var a (Event b)
onlyWhenE' v hot = VarT $ \a -> do
    (e, hot') <- runVarT hot a
    (b, v') <- runVarT v a
    case e of
        Just _ -> return (Just b, onlyWhenE' v' hot')
        _      -> return (Nothing, onlyWhenE' v' hot')

mkNetwork :: EditorData -> Var (Event InputEvent) Scene
mkNetwork startingEd
    =   (,) <$> editorDataA startingEd <*> cmdArr
    >>> var (\(ed, cmd) -> Scene (cmd (graph ed)) (renderEditor ed))
    where
    cmdArr = anyE
        [ use (const Done) (pressedEsc >>> onTrue)
        , use (\g -> Push (mkSimulation g)) (pressedSpace >>> onTrue)
        , pure (event $ const None)]
        >>> startWith (const None)

mousePos :: Var (Event InputEvent) Vector2f
mousePos = foldStream applyEvent (SDL.V2 0 0)
    where
    applyEvent :: Vector2f -> InputEvent -> Vector2f
    applyEvent _  (MouseMoveEvent p) = p
    applyEvent mp _                  = mp

mouseLBDown :: Var (Event InputEvent) Bool
mouseLBDown =
    foldStream (\v ->
        (\case MouseClickEvent _ SDL.ButtonLeft SDL.Pressed  -> True
               MouseClickEvent _ SDL.ButtonLeft SDL.Released -> False
               _ -> v))
        False

mouseRBDown :: Var (Event InputEvent) Bool
mouseRBDown =
    foldStream (\v ->
        (\case MouseClickEvent _ SDL.ButtonRight SDL.Pressed  -> True
               MouseClickEvent _ SDL.ButtonRight SDL.Released -> False
               _ -> v))
        False

mouseLBPressed :: Var (Event InputEvent) (Event Vector2f)
mouseLBPressed =
    onlyWhenE' mousePos (filterE (== True) $ mouseLBDown >>> onUnique)

mouseLBReleased :: Var (Event InputEvent) (Event Vector2f)
mouseLBReleased =
    onlyWhenE' mousePos (filterE (== False) $ mouseLBDown >>> onUnique)

mouseRBPressed :: Var (Event InputEvent) (Event Vector2f)
mouseRBPressed =
    onlyWhenE' mousePos (filterE (== True) $ mouseRBDown >>> onUnique)

mouseRBReleased :: Var (Event InputEvent) (Event Vector2f)
mouseRBReleased =
    onlyWhenE' mousePos (filterE (== False) $ mouseRBDown >>> onUnique)

nodeUnderMouseA :: Var (Vector2f, EditorData) (Event (Int, Vector2f))
nodeUnderMouseA = var (\(mp, ed) -> getNodeAt mp (graph ed))

mouseOnUnselectedNode :: Var (Vector2f, EditorData) (Event (Int, Vector2f))
mouseOnUnselectedNode =
    (nodeUnderMouseA >>> arr fromJust) `onlyWhen'` nodeIsUnselected
    where
    nodeIsUnselected = \(mp, ed) ->
        (== Just False) . fmap (isNodeSelected ed . fst)
                        $ getNodeAt mp (graph ed)

emptyLBPress :: Var (Event InputEvent, EditorData) (Event Vector2f)
emptyLBPress =
    filterE (uncurry emptyPos)
            (first mouseLBPressed >>> arr (\(a, b) -> fmap (, b) a))
        >>> arr (fmap fst)

emptyPos :: Vector2f -> EditorData -> Bool
emptyPos mp ed = ( (getNodeAt mp (graph ed) == Nothing) &&
                   (getPinAt  mp (graph ed) == Nothing) )

doubleLClick :: Var (Event InputEvent) Bool
doubleLClick =
    foldStream (\v ->
        (\case MouseClickEvent 2 SDL.ButtonLeft SDL.Pressed  -> True
               _                                             -> False))
        False

emptyDLClick :: Var (Event InputEvent, EditorData) (Event Vector2f)
emptyDLClick =
    bothE const (onlyWhenE' (var fst >>> mousePos)
                            (first mousePos >>> var (uncurry emptyPos)
                                            >>> onTrue))
                (var fst >>> doubleLClick >>> onTrue)

lastLMClickPos :: Var (Event InputEvent) Vector2f
lastLMClickPos = mouseLBPressed >>> startWith (SDL.V2 0 0)

lastRMClickPos :: Var (Event InputEvent) Vector2f
lastRMClickPos = mouseRBPressed >>> startWith (SDL.V2 0 0)

leftDragA :: Var (Event InputEvent) Vector2f
leftDragA = (-) <$> mousePos <*> lastLMClickPos

rightDragA :: Var (Event InputEvent) Vector2f
rightDragA = (-) <$> mousePos <*> lastRMClickPos

selectionRectA :: Var (Event InputEvent, EditorData) (Maybe Rect2f)
selectionRectA =
    onlyWhenE' (var fst >>> rectA)
               (currentToolA >>> var (== Just Select) >>> onTrue)            
    where
    rectA :: Var (Event InputEvent) Rect2f
    rectA = Rect2f <$> lastLMClickPos <*> leftDragA

pressedEsc :: Var (Event InputEvent) Bool
pressedEsc =
    foldStream (\v ->
        (\case KeyboardEvent SDL.KeycodeEscape SDL.Pressed  -> True
               KeyboardEvent SDL.KeycodeEscape SDL.Released -> False
               _ -> v))
        False

pressedDel :: Var (Event InputEvent) Bool
pressedDel =
    foldStream (\v ->
        (\case KeyboardEvent SDL.KeycodeDelete SDL.Pressed  -> True
               KeyboardEvent SDL.KeycodeDelete SDL.Released -> False
               _ -> v))
        False

pressedSpace :: Var (Event InputEvent) Bool
pressedSpace =
    foldStream (\v ->
        (\case KeyboardEvent SDL.KeycodeSpace SDL.Pressed  -> True
               KeyboardEvent SDL.KeycodeSpace SDL.Released -> False
               _ -> v))
        False

-- For some reason, using `loop` with my arrows seems to get stuck in a <<loop>>
-- Hence, I wrote a hackish version
loopWith :: c -> Var (a, c) (b, c) -> Var a b
loopWith startingC v = VarT $ \a -> do
    ((b, c), v') <- runVarT v (a, startingC)
    return (b, loopWith c v')

editorDataA :: EditorData -> Var (Event InputEvent) EditorData
editorDataA startingEd =
    loopWith startingEd (edArr >>> split)
    where
    edArr = pure EditorData
        <*> edGraph
        <*> edSelectionRect
        <*> edSelectedNodes
        <*> edSelectedPin
        <*> edMousePosition
    split           = var (\x -> (x, x))
    edGraph         = graphA (graph startingEd)
    edSelectionRect = selectionRectA
    edSelectedNodes = selectedNodesA
    edSelectedPin   = selectedPinA
    edMousePosition = arr fst >>> mousePos

selectedPinA :: Var (Event InputEvent, EditorData) (Event PinInfo)
selectedPinA =
    bothE const (first lastLMClickPos
                    >>> var (\(mp, ed) -> getPinAt mp (graph ed)))
                (currentToolA >>> var (== Just Connect) >>> onTrue)

selectedNodesA :: Var (Event InputEvent, EditorData) [(NodeIndex, Vector2f)]
selectedNodesA =
    anyE [ -- if the lmb is released, update the selected nodes
         onlyWhenE' (arr (updateSelectedNodes . snd))
                    (arr fst >>> mouseLBReleased)
           -- if the node under the mouse is not part of the selection,
           -- make this node the sole new selection
         , (bothE const (first mousePos >>> mouseOnUnselectedNode)
                        (arr fst >>> mouseLBPressed))
            >>> var (fmap pure)
         -- If the player has clicked on an empty part, unselect
         -- every node
         , onlyWhenE' (var (const [])) emptyLBPress
         -- If a selection rect is active, and there is at least one node inside
         -- the selection, selected nodes are the ones under the rect
         , onlyWhenE' (var (collectSelectedNodes . snd))
                      selectionRectA
         ]
    >>> startWith []

-- TODO add ability to delete connections
graphA :: EditorGraph -> Var (Event InputEvent, EditorData) EditorGraph
graphA startingGraph =
    anyE [ -- Move node
           onlyWhenE' (first leftDragA >>> var (\(d, ed) -> moveSelectedNodes ed d))
                      (currentToolA >>> var (== Just Move) >>> onTrue)
           -- Connect pins
         , onlyWhenE' (second (var graph)
                        >>> ((pinUnderMouseA &&& selectedPinA) &&& var snd)
                        >>> var (uncurry connect))
                      (bothE const leftReleaseOnPin leftClickOnPin)
          -- Add node
         , onlyWhenE' (first mousePos >>> var (uncurry addNodeAt . fmap graph))
                      emptyDLClick
          -- Delete node
         , onlyWhenE' (var ((\ed -> deleteSelectedNodes
                                        (map fst (selectedNodes ed))
                                        (graph ed))
                            . snd))
                      (var fst >>> pressedDel >>> onTrue)
          -- Tune knobs
         , onlyWhenE' (((first ((,) <$> lastRMClickPos <*> rightDragA)
                            >>> second (var graph)) &&& lastKnobValue)
                                >>> var tuneNode)
                      (currentToolA >>> var (== Just Tune) >>> onTrue)
          -- Delete connections
         , onlyWhenE' ((connectionKnobUnderMouse >>> var fromJust)
                        &&& (var (graph . snd))
                        >>> var (\((n1, n2, _), g) -> deleteConnection n1 n2 g))
                      leftClickOnConnectionKnob
         ]
        >>> startWith startingGraph
    where
    connectionKnobUnderMouse =
        first lastLMClickPos
        >>> second (var graph)
        >>> var (\(mp, g) -> getConnectionKnobAt mp g)
    leftClickOnConnectionKnob =
        bothE const (var fst >>> mouseLBPressed)
              (connectionKnobUnderMouse >>> var (/= Nothing) >>> onTrue)
    tuneNode :: (((Vector2f, Vector2f), EditorGraph), Float) -> EditorGraph
    tuneNode (((mp, (SDL.V2 _ dy)), g), f) =
        case getNodeKnobAt mp g of
            Nothing      ->
                case getConnectionKnobAt mp g of
                    Nothing -> g
                    Just (n1, n2, _) ->
                        tuneConnection n1 n2
                                       (clamp (-1) 1
                                              (f - dy / tuneMouseDistance))
                                       g
            Just (ni, _) ->
                tunePerceptron ni (clamp (-1) 1 (f - dy / tuneMouseDistance)) g
    lastKnobValue :: Var (Event InputEvent, EditorData) Float
    lastKnobValue =
        onlyWhenE' (first ((,) <$> lastRMClickPos <*> rightDragA)
                        >>> second (var graph)
                        >>> var (\((mp, _), g) ->
                                    case getNodeKnobAt mp g of
                                        Nothing ->
                                            case getConnectionKnobAt mp g of
                                                Nothing -> 0
                                                Just (_, _, f) -> f
                                        Just (_, f) -> f))
                   (var fst >>> mouseRBPressed)
            >>> startWith 0
    pinUnderMouseA :: Var (Event InputEvent, EditorGraph) PinInfo
    pinUnderMouseA = first mousePos >>> var (fromJust . uncurry getPinAt)
    selectedPinA :: Var (Event InputEvent, EditorGraph) PinInfo
    selectedPinA = first lastLMClickPos >>> var (fromJust . uncurry getPinAt)
    leftClickOnPin = first lastLMClickPos
                 >>> var (\(mp, ed) -> isJust $ getPinAt mp (graph ed))
                 >>> onTrue
    leftReleaseOnPin = bothE const (first mousePos >>> var (\(mp, ed) ->
                                            isJust $ getPinAt mp (graph ed))
                                        >>> onTrue)
                                   (arr fst >>> mouseLBReleased)

currentToolA :: Var (Event InputEvent, EditorData) (Maybe EditorTool)
currentToolA =
    anyE [ onlyWhenE' (pure Nothing)        (arr fst >>> mouseLBReleased)
         , onlyWhenE' (pure Nothing)        (arr fst >>> mouseRBReleased)
         , onlyWhenE' (pure (Just Connect)) leftPressOnPin
         , onlyWhenE' (pure (Just Move))    leftPressOnNodeA
         , onlyWhenE' (pure (Just Select))  emptyLeftPressA
         , onlyWhenE' (pure (Just Tune))    rightPressOnNodeKnob
         , onlyWhenE' (pure (Just Tune))    rightPressOnonnectionKnob
         ]
         >>> startWith Nothing
    where
    leftPressOnNodeA = bothE const
                             (first lastLMClickPos >>> nodeUnderMouseA
                                 >>> var isJust >>> onTrue)
                             (arr fst >>> mouseLBPressed)
    emptyLeftPressA  = bothE const
                             (first lastLMClickPos >>> var (uncurry emptyPos)
                                 >>> onTrue)
                             (arr fst >>> mouseLBPressed)
    leftPressOnPin   = bothE const
                             (first lastLMClickPos
                                 >>> var (\(mp, ed) -> getPinAt mp (graph ed))
                                 >>> var isJust >>> onTrue)
                             (arr fst >>> mouseLBPressed)
    rightPressOnNodeKnob      = bothE const
                                    (first lastRMClickPos
                                        >>> var (\(mp, ed) ->
                                            getNodeKnobAt mp (graph ed))
                                        >>> var isJust >>> onTrue)
                                    (arr fst >>> mouseRBPressed)
    rightPressOnonnectionKnob = bothE const
                                    (first lastRMClickPos
                                        >>> var (\(mp, ed) ->
                                            getConnectionKnobAt mp (graph ed))
                                        >>> var isJust >>> onTrue)
                                    (arr fst >>> mouseRBPressed)

-- mkNetwork :: EditorData -> InputData 
--            -> MomentIO (Event (StackCommand, EditorData))
-- mkNetwork ed' (InputData events mousePosB) = mdo
--     graphB <- stepper (graph ed') graphE
--     selectedNodesB <- accumB [] selectedNodesE
--     lastClickB <- stepper (SDL.V2 0 0) leftPressAt
--     lastRClickB <- stepper (SDL.V2 0 0) rightPressAt
--     selectionRectB <- switchB (pure Nothing)
--                               (unionWith const
--                                         ((pure Nothing) <$ leftRelease)
--                                         (selectionBoxB <$ emptyLeftPress))
--     nodeKnobUnderMouseR <- stepper Nothing
--                         $ fmap (\(g, mp) -> getNodeKnobAt mp g)
--                         (((,) <$> graphB <*> mousePosB) <@ rightPress)
--     connectionKnobUnderMouseR <- stepper Nothing
--                                $ fmap (\(g, mp) -> getConnectionKnobAt mp g)
--                                (((,) <$> graphB <*> mousePosB) <@ rightPress)
--     connectionKnobUnderMouseL <- stepper Nothing
--                               $ fmap (\(g, mp) -> getConnectionKnobAt mp g)
--                               (((,) <$> graphB <*> mousePosB) <@ leftPress)
--     -- nodeUnderMouse :: Behavior (Maybe Bool) - Is there a node under the
--     -- mouse, and if yes, is it among the selected ones
--     nodeUnderMouse <- stepper Nothing
--             $ fmap (\(ed, mp) -> fmap (isNodeSelected ed . fst)
--                                 $ getNodeAt mp (graph ed))
--                     (((,) <$> editorDataB <*> mousePosB) <@ events)
--     currentToolB <- accumB Nothing currentToolE
--     selectedPinB <- stepper Nothing $
--                         unionWith const
--                                   ( fmap (uncurry getPinAt)
--                                   $ ((,) <$> mousePosB <*> graphB) <@ leftPress)
--                                   ( Nothing <$ leftRelease )
--     pinUnderMouse <- stepper False
--                              ( fmap (\(g, mp) ->
--                                          isJust $ (getPinAt mp g))
--                              $ ((,) <$> graphB <*> mousePosB) <@ events)
--     stackCommandB <- stepper None (Done <$ pressedSpaceE)
--     let dragB = pure (-) <*> mousePosB <*> lastClickB
--         selectionBoxB = pure Just <*> (pure Rect2f <*> lastClickB <*> dragB)
--         editorDataB = pure EditorData <*> graphB
--                                       <*> selectionRectB
--                                       <*> selectedNodesB
--                                       <*> selectedPinB
--                                       <*> mousePosB
--         graphE = foldl1 (unionWith const)
--                     [ moveNodesE
--                     , connectE
--                     , createNodeE
--                     , deleteNodesE
--                     , tuneKnobsE
--                     , deleteConnectionE ]
--         moveNodesE = fmap (\(ed, delta) -> moveSelectedNodes ed delta)
--                         (((,) <$> editorDataB <*> dragB)
--                         <@ (whenE (fmap (== Just Move) currentToolB) events))
--         connectE = fmap (\(ed, mp) ->
--                     connect ( (fromJust . selectedPin $ ed)
--                             , (fromJust . getPinAt mp . graph $ ed))
--                             (graph ed))
--             $ ((,) <$> editorDataB <*> mousePosB)
--                 <@ (whenE (fmap (/= Nothing) selectedPinB)
--                         (whenE pinUnderMouse leftRelease))
--         createNodeE = fmap (uncurry addNodeAt)
--                         ((,) <$> mousePosB <*> graphB) <@ emptyDClickE
--         deleteNodesE =
--             fmap (\(g, sn) -> deleteSelectedNodes (fmap fst sn) g)
--                 (((,) <$> graphB <*> selectedNodesB) <@ pressedDelE)
--         selectedNodesE =
--             unions -- if left button is released, update the selected nodes' pos
--                 [ fmap (const . updateSelectedNodes)
--                         (editorDataB <@ leftRelease)
--                 -- if the node under the mouse is not part os the selection,
--                 -- make this node the sole new selection
--                 , fmap (\(g, mp) -> const [fromJust $ getNodeAt mp g])
--                         (((,) <$> graphB <*> mousePosB) <@
--                             (whenE (fmap ((== Just False))
--                                         nodeUnderMouse) leftPress))
--                 -- If the player has clicked on an empty part, unselect
--                 -- every node
--                 , const [] <$ emptyLeftPress
--                 -- If a selection rect is active, selected nodes are the ones
--                 -- under the rect
--                 , fmap (const . collectSelectedNodes)
--                         (editorDataB <@ (whenE (fmap (not . (== Nothing))
--                                                     selectionRectB) events))]
--         deleteConnectionE =
--             fmap (\(g, Just (n1, n2, _)) -> deleteConnection n1 n2 g)
--             $ (((,) <$> graphB <*> connectionKnobUnderMouseL)
--                 <@ (whenE (fmap (/= Nothing) connectionKnobUnderMouseL)
--                             events))
--         tuneKnobsE = unionWith const tuneNodesE tuneConnectionsE
--         tuneNodesE =
--             fmap (\(g, Just (n, v), SDL.V2 mx my, SDL.V2 lx ly) ->
--                     tunePerceptron n (clamp (-1) 1
--                                         (v - ((my - ly) / tuneMouseDistance)))
--                                     g)
--             $ (((,,,) <$> graphB <*> nodeKnobUnderMouseR <*> mousePosB
--                         <*> lastRClickB)
--                         <@ whenE (fmap (== Just Tune) currentToolB)
--                             (whenE (fmap (/= Nothing) nodeKnobUnderMouseR)
--                                 events))
--         tuneConnectionsE =
--             fmap (\(g, Just (n1, n2, v), SDL.V2 mx my, SDL.V2 lx ly) ->
--                 tuneConnection n1 n2 (clamp (-1) 1
--                                         (v - ((my - ly) / tuneMouseDistance)))
--                                 g)
--             $ (((,,,) <$> graphB <*> connectionKnobUnderMouseR <*> mousePosB
--                         <*> lastRClickB)
--                         <@ whenE (fmap (== Just Tune) currentToolB)
--                             (whenE (fmap (/= Nothing) connectionKnobUnderMouseR)
--                                 events))
--         currentToolE =
--             unions [ (const Nothing)        <$ leftRelease
--                     , (const Nothing)        <$ rightRelease
--                     , (const (Just Connect)) <$ leftPressOnPin
--                     , (const (Just Move))    <$ leftPressOnNode
--                     , (const (Just Select))  <$ emptyLeftPress
--                     , (const (Just Tune))    <$ rightPressOnNodeKnob
--                     , (const (Just Tune))    <$ rightPressOnConnectionKnob ]
--         emptyDClickE :: Event Vector2f
--         emptyDClickE = fmap snd
--                     . filterE (\(g, mp) ->
--                                         (getNodeAt mp g == Nothing)
--                                     && (getPinAt  mp g == Nothing))
--                     $ ((,) <$> graphB <*> mousePosB) <@ doubleClickE
--         doubleClickE :: Event InputEvent
--         doubleClickE =
--             filterE (\case MouseClickEvent 2 SDL.ButtonLeft SDL.Pressed -> True
--                            _ -> False)
--                     events
--         leftPress :: Event InputEvent
--         leftPress =
--             filterE (\case MouseClickEvent 1 SDL.ButtonLeft SDL.Pressed -> True
--                            _ -> False)
--                     events
--         leftPressAt :: Event Vector2f
--         leftPressAt = mousePosB <@ leftPress
--         rightPress :: Event InputEvent
--         rightPress =
--             filterE (\case MouseClickEvent 1 SDL.ButtonRight SDL.Pressed -> True
--                            _ -> False)
--                     events
--         rightPressAt :: Event Vector2f
--         rightPressAt = mousePosB <@ rightPress
--         rightPressOnNodeKnob :: Event (Int, Float)
--         rightPressOnNodeKnob =
--             filterJust
--             . fmap (\(g, mp) -> getNodeKnobAt mp g)
--             $ ((,) <$> graphB <*> mousePosB) <@ rightPress
--         rightPressOnConnectionKnob :: Event (Int, Int, Float)
--         rightPressOnConnectionKnob =
--             filterJust
--             . fmap (\(g, mp) -> getConnectionKnobAt mp g)
--             $ ((,) <$> graphB <*> mousePosB) <@ rightPress
--         rightRelease :: Event InputEvent
--         rightRelease =
--             filterE
--                 (\case MouseClickEvent 1 SDL.ButtonRight SDL.Released -> True
--                        _ -> False) events
--         leftRelease :: Event InputEvent
--         leftRelease =
--             filterE (\case MouseClickEvent 1 SDL.ButtonLeft SDL.Released -> True
--                            _ -> False)
--                     events
--         leftPressOnNode :: Event Vector2f
--         leftPressOnNode =
--             fmap snd
--             . filterE (\(g, mp) -> (isJust $ getNodeAt mp g))
--             $ ((,) <$> graphB <*> mousePosB) <@ leftPress
--         leftPressOnPin :: Event (Int, (PinType, Int, Vector2f))
--         leftPressOnPin =
--             filterJust
--             . fmap (\(g, mp) -> (getPinAt mp g))
--             $ ((,) <$> graphB <*> mousePosB) <@ leftPress
--         emptyLeftPress :: Event Vector2f
--         emptyLeftPress =
--             fmap snd
--             . filterE (\(g, mp) -> (getNodeAt mp g == Nothing)
--                                 && (getPinAt  mp g == Nothing))
--             $ ((,) <$> graphB <*> mousePosB) <@ leftPress
--         pressedDelE :: Event InputEvent
--         pressedDelE =
--             filterE (== KeyboardEvent SDL.KeycodeDelete SDL.Pressed) events
--         pressedSpaceE :: Event InputEvent
--         pressedSpaceE =
--             filterE (== KeyboardEvent SDL.KeycodeSpace SDL.Pressed) events
--         retVal = (,) <$> stackCommandB <*> editorDataB
--     return (retVal <@ events)

