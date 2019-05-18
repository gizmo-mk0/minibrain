module Render where

--
import qualified Data.IntMap        as Map
import qualified SDL
import qualified NanoVG             as NVG
import qualified Graphics.GL.Core32 as GL

import Codec.Picture( PixelRGBA8( .. ), imageData, Image )
import Data.Bits ((.|.))
import qualified Data.Vector.Storable as V
import Foreign.Marshal.Utils
import Foreign.C.Types

import Types
import Globals
import Utils
import GameData
import Scene
import Render.RenderHelper

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

    let scene         = currentScene sd
        sceneImage =
            case scene of
                Title      -> renderTitle
                Briefing   -> renderBriefing
                Editor     -> renderEditor md (SDL.V2 w h)
                Simulation -> renderSimulation
                Quit       -> const blank
    renderImage c $ sceneImage sd

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
        activePin           = (selectedPin . editorData) sd
        selRectImage = case selection of
                            Nothing      -> blank
                            Just selRect -> renderSelection selRect
    in compound $ selRectImage
                : connectionToolLine md activePin
                : map (renderPerceptron True)  selectedPerceptrons
                ++ map (renderPerceptron False) perceptrons
                ++ map renderConnection connections
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
            body = fill (if s then perceptronBodyColor
                              else perceptronSelectedBodyColor) $
                        roundedRectangle (SDL.V2 w h) perceptronBodyRoundness
            pins =
                map (renderPin p) 
                      (zip [0..inputPinCount p - 1] (repeat InputPin) ++
                       zip [0..outputPinCount p - 1] (repeat OutputPin))
        in compound
            -- [ translate (SDL.V2 (x - w / 2) (y - h / 2)) $ body
            -- , translate (SDL.V2 x y) $ compound $
            --     renderKnob (baseLevel p) : pins ]
            [ translate (SDL.V2 x y) $ compound $
                translate (SDL.V2 (-w/2) (-h/2)) body
                    : renderKnob (baseLevel p)
                    : translate (SDL.V2 0 ((- h + perceptronLabelSize) / 2))
                        (fill perceptronLabelColor $
                            text perceptronLabelSize (label p))
                    : pins ]
    renderPin :: Perceptron -> (Int, PinType) -> VectorImage
    renderPin perc (n, t) =
        let (SDL.V2 px py) = getPinRelativePosition perc n t
        in  translate (SDL.V2 (px - pinWidth / 2) (py - pinHeight / 2)) $
                fill pinColor $ rectangle (SDL.V2 pinWidth pinHeight)
    renderKnob :: Float -> VectorImage
    renderKnob v =
        let arcDir    = if v > 0 then CW else CCW
            arcDegree = 3 * pi / 2 + (v * pi / 2)
        in  compound
                [ translate (SDL.V2 (-knobWidth / 2) (-knobHeight / 2)) $
                    fill knobBaseColor $
                        roundedRectangle (SDL.V2 knobWidth knobHeight)
                                        knobRoundness
                , translate (SDL.V2 0 (knobWidth / 6)) $
                    stroke (knobWidth / 6) knobColor $
                        arc (knobWidth / 4) (3 * pi / 2, arcDegree) arcDir
                , translate (SDL.V2 0 (-2 * knobWidth / 12)) $
                    stroke 2 knobColor $
                        line (SDL.V2 0 0) (SDL.V2 0 (2 * knobWidth / 12)) ]
    renderConnection :: (Perceptron, Perceptron, Connection) -> VectorImage
    renderConnection (p1, p2, c) =
        let pStart = getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
            pEnd   = getPinAbsolutePosition p2 (dstPinNumber c) InputPin
            (pos1, pos2, pos3, pos4) = connectionControlpoints pStart pEnd
            midPoint = bezierMidPoint pStart pEnd
        in  compound
                [ stroke connectionWidth pinColor $ bezier pos1 pos2 pos3 pos4
                , translate midPoint $ renderKnob (gain c) ]
    connectionToolLine :: Vector2f -> Maybe (Int, (PinType, Int, Vector2f))
                        -> VectorImage
    connectionToolLine mp (Just (_, (_, _, p))) =
        let (pos1, pos2, pos3, pos4) = connectionControlpoints p mp
        in  stroke connectionWidth pinColor $ bezier pos1 pos2 pos3 pos4
    connectionToolLine _ _ = blank

-- --     -- TODO
-- --     -- renderBackground :: Minibrain ()
-- --     -- renderBackground = undefined

renderSimulation :: SceneData -> VectorImage
renderSimulation = undefined