module Render.RenderHelper where

import qualified SDL
import qualified NanoVG as NVG

import Foreign.C.Types

import Types

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
           | Line      Vector2f Vector2f
           | Bezier    Vector2f Vector2f Vector2f Vector2f

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

line :: Vector2f -> Vector2f -> VectorImage
line p1 p2 = VIShape (Line p1 p2)

bezier :: Vector2f -> Vector2f -> Vector2f -> Vector2f -> VectorImage
bezier p1 p2 p3 p4 = VIShape (Bezier p1 p2 p3 p4)

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
    renderShape c (Line (SDL.V2 x1 y1) (SDL.V2 x2 y2)) = do
        NVG.moveTo c (CFloat x1) (CFloat y1)
        NVG.lineTo c (CFloat x2) (CFloat y2)
    renderShape c (Bezier (SDL.V2 x1 y1) (SDL.V2 x2 y2) (SDL.V2 x3 y3)
                  (SDL.V2 x4 y4)) = do
        NVG.moveTo c (CFloat x1) (CFloat y1)
        NVG.bezierTo c (CFloat x2) (CFloat y2) (CFloat x3) (CFloat y3)
                                   (CFloat x4) (CFloat y4)
        -- TODO
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