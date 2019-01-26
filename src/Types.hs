module Types where

import qualified SDL
import Control.Applicative
import GHC.Word (Word8(..))
import Foreign.C.Types (CInt)
import Data.Maybe (isJust)

type Vector2f = SDL.V2 Float
type Vector2i = SDL.V2 Int
data Rect2f = Rect2f
            { topleft :: Vector2f
            , size    :: Vector2f }

rectAroundPosition :: Vector2f -> Vector2f -> Rect2f
rectAroundPosition pos size = Rect2f (pos - (size / 2)) size

-- Gets rid of negative sizes
normalizeRect :: Rect2f -> Rect2f
normalizeRect (Rect2f (SDL.V2 x y) (SDL.V2 w h)) =
    let left   = min x (x + w)
        right  = max x (x + w)
        top    = max y (y + h)
        bottom = min y (y + h)
    in  Rect2f (SDL.V2 left bottom) (SDL.V2 (right - left) (top - bottom))

pointInRect :: Vector2f -> Rect2f -> Bool
pointInRect (SDL.V2 px py) r =
    let (Rect2f (SDL.V2 left bottom) (SDL.V2 w h)) = normalizeRect r
    in  left < px && px < left + w && bottom < py && py < bottom + h

rectsIntersect :: Rect2f -> Rect2f -> Maybe Rect2f
rectsIntersect r1 r2 =
    let (Rect2f (SDL.V2 left1 bottom1) (SDL.V2 w1 h1)) = normalizeRect r1
        (Rect2f (SDL.V2 left2 bottom2) (SDL.V2 w2 h2)) = normalizeRect r2
        right1  = left1   + w1
        top1    = bottom1 + h1
        right2  = left2   + w2
        top2    = bottom2 + h2
        intersectLeft   = max left1   left2
        intersectRight  = min right1  right2
        intersectTop    = min top1    top2
        intersectBottom = max bottom1 bottom2
    in  if  intersectLeft < intersectRight &&
            intersectTop > intersectBottom
            then Just $ Rect2f (SDL.V2 intersectLeft intersectBottom)
                              (SDL.V2 (intersectRight - intersectLeft)
                                      (intersectTop - intersectBottom))
            else Nothing

rectArea :: Rect2f -> Float
rectArea r =
    let (SDL.V2 w h) = size r
    in  w * h

doRectsIntersect :: Rect2f -> Rect2f -> Bool
doRectsIntersect r1 = isJust . (rectsIntersect r1)