module Render where

import qualified SDL
import qualified NanoVG             as NVG
import qualified Graphics.GL.Core32 as GL
import qualified Data.Text as T
import qualified Data.Set  as S

import Data.Bits       ((.|.))
import Foreign.C.Types (CFloat(..))

import Types (Vector2f)

import Globals

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
           | Text      Float String

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

text :: Float -> String -> VectorImage
text size str = if null str then VIBlank else VIShape (Text size str)

fill :: NVG.Color -> VectorImage -> VectorImage
fill c = VIRenderStyle (Fill c)

stroke :: Float -> NVG.Color -> VectorImage -> VectorImage
stroke w c = VIRenderStyle (Stroke w c)

compound :: [VectorImage] -> VectorImage
compound vis = VICompound vis

blank :: VectorImage
blank = VIBlank

-- A naive code generator for NanoVG
-- It will insert one or two extra, unnecessary calls, but for my purposes it's
-- okay.
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
    renderShape c (Text size str) = do
        NVG.fontSize c (CFloat size)
        NVG.fontFace c (T.pack "regular")
        NVG.textAlign c (S.fromList [NVG.AlignCenter, NVG.AlignMiddle])
        NVG.text c 0 0 (T.pack str)
    applyStyles :: NVG.Context -> [RenderStyle] -> IO () -> IO ()
    applyStyles c ss a = foldr (applyStyle c) a ss
    applyStyle c (Fill color) a = NVG.fillColor c color >> a >> NVG.fill c
    applyStyle c (Stroke w color) a =
        NVG.strokeColor c color >> NVG.strokeWidth c (CFloat w) >> a
                                >> NVG.stroke c
    applyTransforms :: NVG.Context -> [Transform] -> IO () -> IO ()
    applyTransforms _ [] a = a
    applyTransforms c (t:[]) a = applyTransform c t >> a >> NVG.resetTransform c
    applyTransforms c (t:ts) a = applyTransform c t >> applyTransforms c ts a
    applyTransform :: NVG.Context -> Transform -> IO ()
    applyTransform c (Translate (SDL.V2 x y)) =
        NVG.translate c (CFloat x) (CFloat y)
    applyTransform c (Rotate r) = NVG.rotate c (CFloat r)
    applyTransform c (Scale (SDL.V2 x y)) = NVG.scale c (CFloat x) (CFloat y)


