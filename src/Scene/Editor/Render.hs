module Scene.Editor.Render where

import qualified SDL

import Types (Vector2f, Rect2f(..))
import Utils (connectionControlPoints, bezierMidPoint)

import Render
import Scene.Editor.Helper
import Scene.Editor.Globals

renderEditor :: EditorData -> VectorImage
renderEditor ed =
    let perceptrons         = getUnselectedNodes ed
        selectedPerceptrons = getSelectedNodes ed
        connections         = edges ed
        selection           = selectionRect ed
        activePin           = selectedPin ed
        mp                  = mousePosition ed
        selRectImage = case selection of
                            Nothing      -> blank
                            Just selRect -> renderSelection selRect
    in compound $ selRectImage
                : connectionToolLine mp activePin
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
        let pw           = perceptronWidth
            ph           = getPerceptronHeight p
            (SDL.V2 x y) = position p
            body = fill (if s then perceptronBodyColor
                              else perceptronSelectedBodyColor) $
                        roundedRectangle (SDL.V2 pw ph) perceptronBodyRoundness
            pins =
                map (renderPin p) 
                      (zip [0..inputPinCount p - 1] (repeat InputPin) ++
                       zip [0..outputPinCount p - 1] (repeat OutputPin))
        in compound
            [ translate (SDL.V2 x y) $ compound $
                translate (SDL.V2 (-pw/2) (-ph/2)) body
                    : (if null (label p)
                            then renderKnob (baseLevel p)
                            else fill perceptronLabelColor $
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
            (pos1, pos2, pos3, pos4) = connectionControlPoints pStart pEnd
            midPoint = bezierMidPoint pStart pEnd
        in  compound
                [ stroke connectionWidth pinColor $ bezier pos1 pos2 pos3 pos4
                , translate midPoint $ renderKnob (gain c) ]
    connectionToolLine :: Vector2f -> Maybe (Int, (PinType, Int, Vector2f))
                        -> VectorImage
    connectionToolLine mp (Just (_, (_, _, p))) =
        stroke connectionWidth pinColor $ line mp p
    connectionToolLine _ _ = blank