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

-- isPointInPoly :: [Vector2f] -> Vector2f -> Bool
-- isPointInPoly poly p@(SDL.V2 x y) =
--     let minX = minimum (map (^.SDL._x) poly)
--         maxX = maximum (map (^.SDL._x) poly)
--         minY = minimum (map (^.SDL._y) poly)
--         maxY = maximum (map (^.SDL._y) poly)
--         sides = zip poly (drop 1 (cycle poly))
--         isEven = (== 0) . (`mod` 2)
--     in  if x < minX || x > maxX || y < minY || y > maxY
--             then False
--             else not . isEven . length
--                . filter (doIntersect ((SDL.V2 (minX - 1) y), p))
--                $ sides

-- lineToPoly :: Float -> [Vector2f] -> [[Vector2f]]
-- lineToPoly _ []     = error "lineToPoly called with empty list"
-- lineToPoly _ (_:[]) = error "lineToPoly called with one element"
-- lineToPoly thickness line' =
--     let halfThickness = thickness / 2
--         duplicateFirst (x1:x2:xs) = (2 * x1 - x2):x1:x2:xs
--         line = reverse . duplicateFirst . reverse . duplicateFirst $ line'
--         normals = map (fmap (* halfThickness) . SDL.signorm . SDL.perp)
--                 . zipWith (-) line $ (drop 1 line)
--         avgNormals  = map (fmap (/2)) . zipWith (+) normals . drop 1 $ normals
--         leftPoints  = zipWith (+) line' avgNormals
--         rightPoints = zipWith (-) line' avgNormals
--         pointPairs  = zip leftPoints rightPoints
--         pointQuads  = zip pointPairs (drop 1 pointPairs)
--     in  map (\((p1, p2), (p4, p3)) -> [p1, p2, p3, p4]) pointQuads

-- -- does the infinite horizontal line, starting from p, intersect the p1-p2 line
-- -- segment?
-- doIntersect :: (Vector2f, Vector2f) -> (Vector2f, Vector2f) -> Bool
-- doIntersect (SDL.V2 v1x1 v1y1, SDL.V2 v1x2 v1y2)
--             (SDL.V2 v2x1 v2y1, SDL.V2 v2x2 v2y2) =
--     let a1 = v1y2 - v1y1
--         b1 = v1x1 - v1x2
--         c1 = (v1x2 * v1y1) - (v1x1 * v1y2)
--         d1 = (a1 * v2x1) + (b1 * v2y1) + c1
--         d2 = (a1 * v2x2) + (b1 * v2y2) + c1
--         a2 = v2y2 - v2y1
--         b2 = v2x1 - v2x2
--         c2 = (v2x2 * v2y1) - (v2x1 * v2y2)
--         d3 = (a2 * v1x1) + (b2 * v1y1) + c2
--         d4 = (a2 * v1x2) + (b2 * v1y2) + c2
--     in  if (d1 > 0 && d2 > 0) || (d1 < 0 && d2 < 0)
--             then False
--             else if (d3 > 0 && d4 > 0) || (d3 < 0 && d4 < 0)
--                 then False
--                 else True

-- circleSolid :: Float -> G.Picture
-- circleSolid r = G.ThickCircle (r / 2) r

-- thickLine :: Vector2f -> Vector2f -> Float -> G.Picture
-- thickLine start@(SDL.V2 x1 y1) end@(SDL.V2 x2 y2) w = G.Polygon points
--     where
--     (SDL.V2 x y) = end - start
--     normal = SDL.V2 (-y * (w / 2) / l) (x * (w / 2) / l)
--     l = sqrt (x * x + y * y)
--     points = map (\(SDL.V2 c1 c2) -> (c1, c2))
--                  [start + normal, end + normal, end - normal, start - normal]

-- -- thickCurve :: Float -> [Vector2f] -> G.Picture
-- -- thickCurve thickness points = G.Polygon . map (\(SDL.V2 x y) -> (x, y))
-- --                                         $ lineToPoly thickness $ points

-- thickCurve :: Float -> [Vector2f] -> G.Picture
-- thickCurve thickness points = G.Pictures
--                             . map (G.Polygon . map (\(SDL.V2 x y) -> (x, y)))
--                             . lineToPoly thickness
--                             $ points

-- fillRoundRectangle :: Vector2f -> Float -> G.Picture
-- fillRoundRectangle (SDL.V2 w h) r =
--     G.Pictures $ G.Polygon points
--                : map (\(dx, dy) -> G.Translate dx dy (circleSolid r))
--                      circlePositions
--     where
--     points = [ (-(w / 2) + r, -(h / 2))
--              , ( (w / 2) - r, -(h / 2))
--              , ( (w / 2),     -(h / 2) + r)
--              , ( (w / 2),      (h / 2) - r)
--              , ( (w / 2) - r,  (h / 2))
--              , (-(w / 2) + r,  (h / 2))
--              , (-(w / 2),      (h / 2) - r)
--              , (-(w / 2),     -(h / 2) + r)]
--     circlePositions = [ (-(w / 2) + r, -(h / 2) + r)
--                       , ( (w / 2) - r, -(h / 2) + r)
--                       , (-(w / 2) + r,  (h / 2) - r)
--                       , ( (w / 2) - r,  (h / 2) - r) ]

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
        -- img = renderDrawing w h editorBackgroundColor $
        --     withTexture (uniformTexture backgroundLines) $ do
        --         fill $ circle (V2 0 0) 30
        --         stroke 4 JoinRound (CapRound, CapRound) $
        --             circle (V2 500 0) 40
        --         withTexture (uniformTexture backgroundLines) .
        --             fill $ rectangle (V2 100 100) 200 100
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
        -- selectionShape =
        --     case selection of
        --         Nothing -> G.Blank
        --         Just selRect -> renderSelection selRect
        activePin = (selectedPin . editorData) sd
    -- renderBackground
    mapM_ (renderPerceptron True)  selectedPerceptrons
    mapM_ (renderPerceptron False) perceptrons
--     in  G.Color background
--         --    $ renderBackground
--            $ G.Pictures $ map (renderPerceptron True)  selectedPerceptrons
--                        ++ map renderConnection connections
--                        ++ map (renderPerceptron False) perceptrons
--                        ++ [selectionShape, connectionToolLine md activePin]
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
            -- knob = renderKnob (baseLevel p)
        withTransformation (translate (V2 (x - w / 2) (y - h / 2))) $ body
        withTransformation (translate (V2 x y)) $ pins
    renderPin :: Perceptron -> (Int, PinType) -> Drawing PixelRGBA8 ()
    renderPin perc (n, t) = do
        let (SDL.V2 px py) = getPinRelativePosition perc n t
        withTransformation (translate (V2 (px - pinWidth / 2)
                                          (py - pinHeight / 2))) $
            withTexture (uniformTexture pinColor) $
                fill $ rectangle (V2 0 0) pinWidth pinHeight
--     renderKnob :: Float -> G.Picture
--     renderKnob v =
--         G.Pictures [ G.Color knobBaseColor $
--                         fillRoundRectangle (SDL.V2 knobWidth knobHeight)
--                                            knobRoundness
--                    , G.Translate 0 (-knobWidth / 6) $ G.Color knobColor $
--                         G.ThickArc 90 (90 - v * 90)
--                                    (knobWidth / 4)
--                                    (knobWidth / 6)
--                    , G.Translate 0 (-knobWidth / 6) $ G.Color knobColor $
--                         thickLine (SDL.V2 0 (knobWidth / 4 - knobWidth / 12))
--                                   (SDL.V2 0 (knobWidth / 4 + knobWidth / 12)) 2]
--     renderConnection :: (Perceptron, Perceptron, Connection) -> G.Picture
--     renderConnection (p1, p2, c) =
--         let pos1@(SDL.V2 x1 y1) =
--                 getPinAbsolutePosition p1 (srcPinNumber c) OutputPin
--             pos2@(SDL.V2 x2 y2) =
--                 getPinAbsolutePosition p2 (dstPinNumber c) InputPin
--             (points, (SDL.V2 mx my)) = mkCurveWithMidpoint pos1 pos2
--         in G.Color pinColor $
--                 G.Pictures $
--                     [ thickCurve connectionWidth points
--                     , G.Translate mx my $ renderKnob (gain c)]
        
--     -- TODO
--     -- renderBackground :: Minibrain ()
--     -- renderBackground = undefined
--     renderSelection :: Rect2f -> G.Picture
--     renderSelection (Rect2f (SDL.V2 left top) (SDL.V2 w h)) =
--         let points = [(0, 0), (w, 0), (w, h), (0, h)]
--             coords = zip points (drop 1 (cycle points))
--         in  G.Translate left top $ G.Pictures
--                 [ G.Color selectionLineColor $ G.Pictures $
--                         map (\((p1x, p1y), (p2x, p2y)) ->
--                                 thickLine (SDL.V2 p1x p1y)
--                                           (SDL.V2 p2x p2y) 1) coords
--                 , G.Color selectionFillColor $ G.Polygon points ]
--     connectionToolLine :: Vector2f -> Maybe (Int, (PinType, Int, Vector2f))
--                        -> G.Picture
--     connectionToolLine md (Just (_, (_, _, pp))) =
--         G.Color pinColor $ thickLine md pp connectionWidth
--     connectionToolLine _ _ = G.Blank

renderSimulation :: SceneData -> Image PixelRGBA8
renderSimulation = undefined