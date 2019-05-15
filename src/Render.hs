module Render where

--
import qualified Data.IntMap        as Map
import qualified SDL
import qualified NanoVG             as NVG
import qualified Graphics.GL.Core32 as GL

-- import Control.Monad.State.Strict (gets)
-- import Control.Monad.Reader (asks, liftIO)
-- import Control.Lens ((^.))
-- import Linear.Metric (norm)
-- import Data.Fixed (mod')

import Codec.Picture( PixelRGBA8( .. ), imageData, Image )
import Data.Bits ((.|.))
-- import Graphics.Rasterific
-- import Graphics.Rasterific.Texture
-- import Graphics.Rasterific.Transformations
import qualified Data.Vector.Storable as V
import Foreign.Marshal.Utils
import Foreign.C.Types

import Types
import Globals
import Utils
import GameData
import Scene

data Winding = CW | CCW deriving (Eq)

data VectorImage = VIShape Shape
                 | VIRenderStyle RenderStyle VectorImage
                 | VITransform   Transform   VectorImage
                 | VICompound    [VectorImage]
                 | VIBlank

data Shape = RoundedRectangle Vector2f Float
           | Rectangle Vector2f
           | Circle    Float
           | Ellipse   Vector2f
           | Arc       Float (Float, Float) Winding

data RenderStyle = Fill   NVG.Color
                 | Stroke Float NVG.Color

data Transform = Translate Vector2f
               | Rotate    Float
               | Scale     Vector2f

translate :: Vector2f -> VectorImage -> VectorImage
translate v = VITransform (Translate v)

rotate :: Float -> VectorImage -> VectorImage
rotate r = VITransform (Rotate r)

scale :: Vector2f -> VectorImage -> VectorImage
scale v = VITransform (Scale v)

roundedRectangle :: Vector2f -> Float -> VectorImage
roundedRectangle size r = VIShape (RoundedRectangle size r)

rectangle :: Vector2f -> VectorImage
rectangle size = VIShape (Rectangle size)

circle :: Float -> VectorImage
circle r = VIShape (Circle r)

ellipse :: Vector2f -> VectorImage
ellipse rs = VIShape (Ellipse rs)

arc :: Float -> (Float, Float) -> Winding -> VectorImage
arc r as w = VIShape (Arc r as w)

fill :: NVG.Color -> VectorImage -> VectorImage
fill c = VIRenderStyle (Fill c)

stroke :: Float -> NVG.Color -> VectorImage -> VectorImage
stroke w c = VIRenderStyle (Stroke w c)

compound :: [VectorImage] -> VectorImage
compound vis = VICompound vis

blank :: VectorImage
blank = VIBlank

renderImage :: NVG.Context -> VectorImage -> IO ()
renderImage c vi = renderImage' c [] [] vi
    where
    renderImage' :: NVG.Context -> [Transform] -> [RenderStyle] -> VectorImage
                 -> IO ()
    renderImage' _ _ _ VIBlank = return ()
    renderImage' c ts rs (VITransform t vi) = renderImage' c (t:ts) rs vi
    renderImage' c ts rs (VIRenderStyle r vi) = renderImage' c ts (r:rs) vi
    renderImage' c ts rs (VICompound vis) = mapM_ (renderImage' c ts rs) vis
    renderImage' c ts rs (VIShape s) = do
        NVG.beginPath c
        applyTransforms c ts $ applyStyles c rs $ renderShape c s
    renderShape :: NVG.Context -> Shape -> IO ()
    renderShape c (RoundedRectangle (SDL.V2 w h) r) =
        NVG.roundedRect c 0 0 (CFloat w) (CFloat h) (CFloat r)
    renderShape c (Rectangle (SDL.V2 w h)) =
        NVG.rect c 0 0 (CFloat w) (CFloat h)
    renderShape c (Circle r) = NVG.circle c 0 0 (CFloat r)
    renderShape c (Ellipse (SDL.V2 r1 r2)) =
        NVG.ellipse c 0 0 (CFloat r1) (CFloat r2)
    renderShape c (Arc r (a1, a2) w) = do
        let winding = case w of
                        CW  -> NVG.CW
                        CCW -> NVG.CCW
        NVG.arc c 0 0 (CFloat r) (CFloat a1) (CFloat a2) winding
    applyStyles :: NVG.Context -> [RenderStyle] -> IO () -> IO ()
    applyStyles c ss a = a >> mapM_ (applyStyle c) ss
    applyStyle c (Fill color) = NVG.fillColor c color >> NVG.fill c
    applyStyle c (Stroke w color) =
        NVG.strokeColor c color >> NVG.strokeWidth c (CFloat w) >> NVG.stroke c
    applyTransforms :: NVG.Context -> [Transform] -> IO () -> IO ()
    applyTransforms _ [] a = a
    applyTransforms c (t:[]) a = applyTransform c t >> a >> NVG.resetTransform c
    applyTransforms c (t:ts) a = applyTransform c t >> applyTransforms c ts a
    applyTransform :: NVG.Context -> Transform -> IO ()
    applyTransform c (Translate (SDL.V2 x y)) =
        NVG.translate c (CFloat x) (CFloat y)
    applyTransform c (Rotate r) = NVG.rotate c (CFloat r)
    applyTransform c (Scale (SDL.V2 x y)) = NVG.scale c (CFloat x) (CFloat y)


renderCurrentScene :: Config -> GameData -> IO ()
renderCurrentScene (Config window (SDL.V2 w h) glc c)
                   (GameData sd (CameraData (SDL.V2 dx dy) rotation zoom) md)
                   = do
    let (NVG.Color (CFloat bg_r) (CFloat bg_g)
                   (CFloat bg_b) (CFloat bg_a)) = editorBackgroundColor
    GL.glClearColor bg_r bg_g bg_b bg_a
    GL.glClear (   GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT
                .|. GL.GL_STENCIL_BUFFER_BIT)
    NVG.beginFrame c (fromIntegral w) (fromIntegral h) 1

    -- NVG.strokeColor c (NVG.Color 1 1 1 1)
    -- bg <- NVG.linearGradient c 250 100 265 200 (NVG.rgba 255 0 0 32) (NVG.rgba 0 255 0 16)
    -- NVG.beginPath c
    -- NVG.ellipse c 200 200 200 200
    -- NVG.fillPaint c bg
    -- NVG.fill c

    let scene         = currentScene sd
        sceneImage =
            case scene of
                Title      -> renderTitle
                Briefing   -> renderBriefing
                Editor     -> renderEditor md (SDL.V2 w h)
                Simulation -> renderSimulation
                Quit       -> const blank
    renderImage c $ sceneImage sd

    -- renderImage c $
    --     fill selectionFillColor $
    --         stroke 1 selectionLineColor $
    --             circle 200

    NVG.endFrame c
    SDL.glSwapWindow window

    -- -- TODO make zoom relative to the screen center instead of the world center
    -- liftIO $ G.displayPicture (w, h) (G.makeColor 0 0 0 0) s zoom
    --        $ G.Translate dx dy
    --        $ G.Rotate rotation
    --        $ G.Scale zoom zoom
    --        $ sceneGeometry
    --        $ sd

renderTitle :: SceneData -> VectorImage
renderTitle = undefined

renderBriefing :: SceneData -> VectorImage
renderBriefing = undefined

renderEditor :: Vector2f -> Vector2i -> SceneData -> VectorImage
renderEditor md (SDL.V2 w h) sd =
    let perceptrons         = (getUnselectedNodes . editorData) sd
        selectedPerceptrons = (getSelectedNodes . editorData) sd
        connections         = (edges . editorData) sd
        selection           = (selectionRect . editorData) sd
        activePin = (selectedPin . editorData) sd
        selRectImage = case selection of
                            Nothing      -> blank
                            Just selRect -> renderSelection selRect
    in compound $ selRectImage
                : map (renderPerceptron True)  selectedPerceptrons
                ++ map (renderPerceptron False) perceptrons
    where
    renderSelection :: Rect2f -> VectorImage
    renderSelection (Rect2f pos size) =
        fill selectionFillColor $ stroke 1 selectionLineColor $ translate pos $
            rectangle size
    renderPerceptron :: Bool -> Perceptron -> VectorImage
    renderPerceptron s p =
        let w            = perceptronWidth
            h            = getPerceptronHeight p
            (SDL.V2 x y) = position p
            body =
                translate (SDL.V2 (x - w / 2) (y - h / 2)) $
                    fill (if s then perceptronBodyColor
                               else perceptronSelectedBodyColor) $
                        roundedRectangle (SDL.V2 w h) perceptronBodyRoundness
            -- pins = do
            --     mapM_ (renderPin p) 
            --           (zip [0..inputPinCount p - 1] (repeat InputPin) ++
            --            zip [0..outputPinCount p - 1] (repeat OutputPin))
        in
        -- withTransformation (translate (V2 (x - w / 2) (y - h / 2))) $ body
            body
        -- withTransformation (translate (V2 x y)) $ do
            -- renderKnob (baseLevel p)
            -- pins

-- renderEditor :: Vector2f -> Vector2i -> SceneData -> Image PixelRGBA8
-- renderEditor md (SDL.V2 w h) sd = renderDrawing w h editorBackgroundColor $
--     withTexture (uniformTexture backgroundLines) $ do
--     let perceptrons         = (getUnselectedNodes . editorData) sd
--         selectedPerceptrons = (getSelectedNodes . editorData) sd
--         connections         = (edges . editorData) sd
--         selection           = (selectionRect . editorData) sd
--         activePin = (selectedPin . editorData) sd
--     case selection of
--         Nothing      -> return ()
--         Just selRect -> renderSelection selRect
--     -- renderBackground
--     mapM_ (renderPerceptron True)  selectedPerceptrons
--     mapM_ (renderPerceptron False) perceptrons
--     mapM_ renderConnection connections
--     connectionToolLine md activePin
--     where
--     renderPerceptron :: Bool -> Perceptron -> Drawing PixelRGBA8 ()
--     renderPerceptron s p = do
--         let w            = perceptronWidth
--             h            = getPerceptronHeight p
--             (SDL.V2 x y) = position p
--             body = withTexture
--                         (uniformTexture
--                             (if s then perceptronBodyColor
--                                   else perceptronSelectedBodyColor))
--                         (fill $ roundedRectangle (V2 0 0) w h
--                                                   perceptronBodyRoundness
--                                                   perceptronBodyRoundness)
--             pins = do
--                 mapM_ (renderPin p) 
--                       (zip [0..inputPinCount p - 1] (repeat InputPin) ++
--                        zip [0..outputPinCount p - 1] (repeat OutputPin))
--         withTransformation (translate (V2 (x - w / 2) (y - h / 2))) $ body
--         withTransformation (translate (V2 x y)) $ do
--             renderKnob (baseLevel p)
--             pins
--     renderPin :: Perceptron -> (Int, PinType) -> Drawing PixelRGBA8 ()
--     renderPin perc (n, t) = do
--         let (SDL.V2 px py) = getPinRelativePosition perc n t
--         withTransformation (translate (V2 (px - pinWidth / 2)
--                                           (py - pinHeight / 2))) $
--             withTexture (uniformTexture pinColor) $
--                 fill $ rectangle (V2 0 0) pinWidth pinHeight
--     renderKnob :: Float -> Drawing PixelRGBA8 ()
--     renderKnob v = do
--         let arcDir    = if v > 0 then Forward else Backward
--             arcDegree = pi / 2 + (v * pi / 2)
--             arcStart  = min (pi / 2) arcDegree
--             arcEnd    = max (pi / 2) arcDegree
--         withTransformation
--             (translate (V2 (-knobWidth / 2) (-knobHeight / 2))) $
--             withTexture (uniformTexture knobBaseColor) $
--                 fill $ roundedRectangle (V2 0 0) knobWidth knobHeight
--                                         knobRoundness knobRoundness
--         withTransformation (translate (V2 0 (knobWidth / 6))) $
--             withTexture (uniformTexture knobColor) $
--                 stroke (knobWidth / 6) JoinRound
--                         (CapStraight 0, CapStraight 0)
--                     $ Path (V2 0 (-(knobWidth / 4))) False $
--                         arcInDirection (V2 0 0) (knobWidth / 4) arcDir 0.1
--                                     (pi + arcStart) (pi + arcEnd)
--         withTransformation (translate (V2 0 (-knobWidth / 12))) $
--             withTexture (uniformTexture knobColor) $
--                 stroke (knobWidth / 6) JoinRound (CapStraight 0, CapStraight 0)
--                     $ line (V2 (- 1) 0) (V2 1 0)
--     renderConnection :: (Perceptron, Perceptron, Connection)
--                      -> Drawing PixelRGBA8 ()
--     renderConnection (p1, p2, c) =
--         let pos1@(SDL.V2 x1 y1) =
--                 getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
--             pos2@(SDL.V2 x2 y2) =
--                 getPinAbsolutePosition p2 (dstPinNumber c) InputPin
--             (points, (SDL.V2 mx my)) = mkCurveWithMidpoint pos1 pos2
--         in withTexture (uniformTexture pinColor) $ do
--                 stroke connectionWidth JoinRound
--                        (CapStraight 0, CapStraight 0) $
--                     polyline (map (\(SDL.V2 x y) -> V2 x y) points)
--                 withTransformation (translate (V2 mx my)) $
--                     renderKnob (gain c)
        
-- --     -- TODO
-- --     -- renderBackground :: Minibrain ()
-- --     -- renderBackground = undefined
--     renderSelection :: Rect2f -> Drawing PixelRGBA8 ()
--     renderSelection (Rect2f (SDL.V2 left top) (SDL.V2 w h)) = do
--         withTexture (uniformTexture selectionLineColor) $
--             stroke 1 JoinRound (CapStraight 0, CapStraight 0) $
--                 rectangle (V2 left top) w h
--         withTexture (uniformTexture selectionFillColor) $
--             fill $ rectangle (V2 left top) w h
--     connectionToolLine :: Vector2f -> Maybe (Int, (PinType, Int, Vector2f))
--                        -> Drawing PixelRGBA8 ()
--     connectionToolLine (SDL.V2 mx my) (Just (_, (_, _, (SDL.V2 px py)))) = do
--         withTexture (uniformTexture pinColor) $
--             stroke connectionWidth JoinRound (CapStraight 0, CapStraight 0) $
--                 line (V2 mx my) (V2 px py)
--     connectionToolLine _ _ = return ()

renderSimulation :: SceneData -> VectorImage
renderSimulation = undefined