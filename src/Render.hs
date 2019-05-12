module Render where

--
import qualified Data.IntMap as Map
import qualified SDL

-- import Control.Monad.State.Strict (gets)
-- import Control.Monad.Reader (asks, liftIO)
-- import Control.Lens ((^.))
-- import Linear.Metric (norm)
-- import Data.Fixed (mod')

import Codec.Picture( PixelRGBA8( .. ), imageData, Image )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import qualified Data.Vector.Storable as V
import Foreign.Marshal.Utils
import Foreign.Ptr

import Types
import Globals
import Utils
import GameData
import Scene

renderCurrentScene :: Config -> GameData -> IO ()
renderCurrentScene (Config window (SDL.V2 w h) renderer texture)
                   (GameData sd (CameraData (SDL.V2 dx dy) rotation zoom) md)
                   = do
    let scene         = currentScene sd
        sceneGeometry =
            case scene of
                Title      -> renderTitle
                Briefing   -> renderBriefing
                Editor     -> renderEditor md (SDL.V2 w h)
                Simulation -> renderSimulation
                Quit       -> return $ renderDrawing w h background $ return ()
    SDL.clear renderer
    (texPtr, pitch) <- SDL.lockTexture texture Nothing
    V.unsafeWith (imageData (sceneGeometry sd)) $ \p ->
        copyBytes texPtr (castPtr p) (h * fromIntegral pitch)
    SDL.unlockTexture texture
    SDL.copy renderer texture Nothing Nothing
    SDL.present renderer

    -- -- TODO make zoom relative to the screen center instead of the world center
    -- liftIO $ G.displayPicture (w, h) (G.makeColor 0 0 0 0) s zoom
    --        $ G.Translate dx dy
    --        $ G.Rotate rotation
    --        $ G.Scale zoom zoom
    --        $ sceneGeometry
    --        $ sd

renderTitle :: SceneData -> Image PixelRGBA8
renderTitle = undefined

renderBriefing :: SceneData -> Image PixelRGBA8
renderBriefing = undefined

renderEditor :: Vector2f -> Vector2i -> SceneData -> Image PixelRGBA8
renderEditor md (SDL.V2 w h) sd = renderDrawing w h editorBackgroundColor $
    withTexture (uniformTexture backgroundLines) $ do
    let perceptrons         = (getUnselectedNodes . editorData) sd
        selectedPerceptrons = (getSelectedNodes . editorData) sd
        connections         = (edges . editorData) sd
        selection           = (selectionRect . editorData) sd
        activePin = (selectedPin . editorData) sd
    case selection of
        Nothing      -> return ()
        Just selRect -> renderSelection selRect
    -- renderBackground
    mapM_ (renderPerceptron True)  selectedPerceptrons
    mapM_ (renderPerceptron False) perceptrons
    mapM_ renderConnection connections
    connectionToolLine md activePin
    where
    renderPerceptron :: Bool -> Perceptron -> Drawing PixelRGBA8 ()
    renderPerceptron s p = do
        let w            = perceptronWidth
            h            = getPerceptronHeight p
            (SDL.V2 x y) = position p
            body = withTexture
                        (uniformTexture
                            (if s then perceptronBodyColor
                                  else perceptronSelectedBodyColor))
                        (fill $ roundedRectangle (V2 0 0) w h
                                                  perceptronBodyRoundness
                                                  perceptronBodyRoundness)
            pins = do
                mapM_ (renderPin p) 
                      (zip [0..inputPinCount p - 1] (repeat InputPin) ++
                       zip [0..outputPinCount p - 1] (repeat OutputPin))
        withTransformation (translate (V2 (x - w / 2) (y - h / 2))) $ body
        withTransformation (translate (V2 x y)) $ do
            renderKnob (baseLevel p)
            pins
    renderPin :: Perceptron -> (Int, PinType) -> Drawing PixelRGBA8 ()
    renderPin perc (n, t) = do
        let (SDL.V2 px py) = getPinRelativePosition perc n t
        withTransformation (translate (V2 (px - pinWidth / 2)
                                          (py - pinHeight / 2))) $
            withTexture (uniformTexture pinColor) $
                fill $ rectangle (V2 0 0) pinWidth pinHeight
    renderKnob :: Float -> Drawing PixelRGBA8 ()
    renderKnob v = do
        let arcDir    = if v > 0 then Forward else Backward
            arcDegree = pi / 2 + (v * pi / 2)
            arcStart  = min (pi / 2) arcDegree
            arcEnd    = max (pi / 2) arcDegree
        withTransformation
            (translate (V2 (-knobWidth / 2) (-knobHeight / 2))) $
            withTexture (uniformTexture knobBaseColor) $
                fill $ roundedRectangle (V2 0 0) knobWidth knobHeight
                                        knobRoundness knobRoundness
        withTransformation (translate (V2 0 (knobWidth / 6))) $
            withTexture (uniformTexture knobColor) $
                stroke (knobWidth / 6) JoinRound
                        (CapStraight 0, CapStraight 0)
                    $ Path (V2 0 (-(knobWidth / 4))) False $
                        arcInDirection (V2 0 0) (knobWidth / 4) arcDir 0.1
                                    (pi + arcStart) (pi + arcEnd)
        withTransformation (translate (V2 0 (-knobWidth / 12))) $
            withTexture (uniformTexture knobColor) $
                stroke (knobWidth / 6) JoinRound (CapStraight 0, CapStraight 0)
                    $ line (V2 (- 1) 0) (V2 1 0)
    renderConnection :: (Perceptron, Perceptron, Connection)
                     -> Drawing PixelRGBA8 ()
    renderConnection (p1, p2, c) =
        let pos1@(SDL.V2 x1 y1) =
                getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
            pos2@(SDL.V2 x2 y2) =
                getPinAbsolutePosition p2 (dstPinNumber c) InputPin
            (points, (SDL.V2 mx my)) = mkCurveWithMidpoint pos1 pos2
        in withTexture (uniformTexture pinColor) $ do
                stroke connectionWidth JoinRound
                       (CapStraight 0, CapStraight 0) $
                    polyline (map (\(SDL.V2 x y) -> V2 x y) points)
                withTransformation (translate (V2 mx my)) $
                    renderKnob (gain c)
        
--     -- TODO
--     -- renderBackground :: Minibrain ()
--     -- renderBackground = undefined
    renderSelection :: Rect2f -> Drawing PixelRGBA8 ()
    renderSelection (Rect2f (SDL.V2 left top) (SDL.V2 w h)) = do
        withTexture (uniformTexture selectionLineColor) $
            stroke 1 JoinRound (CapStraight 0, CapStraight 0) $
                rectangle (V2 left top) w h
        withTexture (uniformTexture selectionFillColor) $
            fill $ rectangle (V2 left top) w h
    connectionToolLine :: Vector2f -> Maybe (Int, (PinType, Int, Vector2f))
                       -> Drawing PixelRGBA8 ()
    connectionToolLine (SDL.V2 mx my) (Just (_, (_, _, (SDL.V2 px py)))) = do
        withTexture (uniformTexture pinColor) $
            stroke connectionWidth JoinRound (CapStraight 0, CapStraight 0) $
                line (V2 mx my) (V2 px py)
    connectionToolLine _ _ = return ()

renderSimulation :: SceneData -> Image PixelRGBA8
renderSimulation = undefined