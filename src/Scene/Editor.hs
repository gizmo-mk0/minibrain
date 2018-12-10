{-# LANGUAGE DuplicateRecordFields #-}

module Scene.Editor where

--
import qualified SDL
import qualified Data.IntMap as Map
import Foreign.C.Types (CInt)

import Types
import Globals

data EditorData = EditorData
                { perceptrons :: [Perceptron]
                , connectors  :: [Connector]}

data Perceptron = Perceptron
                { ix       :: Int
                , position :: Vector2
                , pins     :: [Pin] }
data Pin = Pin
         { ix       :: Int
         , getType  :: PinType
         , parentId :: Int }
data PinType = InputPin | OutputPin deriving (Eq)
data Connector = Connector Int Int

defaultEditorData :: EditorData
defaultEditorData =
    EditorData [Perceptron 0
                           (SDL.V2 300 400)
                           [ Pin 0 InputPin  1
                           , Pin 0 OutputPin 1
                           , Pin 1 InputPin  1]]
                []

getPerceptronHeight :: Perceptron -> CInt
getPerceptronHeight p =
    perceptronBodyRoundness * 2 + moduleCount * perceptronModuleHeight
    where
    moduleCount    = fromIntegral $ max inputPinCount outputPinCount
    inputPinCount  = length $ filter (\(Pin _ t _) -> t == InputPin)  (pins p)
    outputPinCount = length $ filter (\(Pin _ t _) -> t == OutputPin) (pins p)

getPinPosition :: Perceptron -> Pin -> Vector2
getPinPosition perc@(Perceptron _ pos@(SDL.V2 px py) _) pin@(Pin ix pinType _) =
    let verticalPos  = -(parentHeight `div` 2)
                     + perceptronBodyRoundness
                     + (fromIntegral ix * perceptronModuleHeight)
                     + (perceptronModuleHeight `div` 2)
        parentHeight = getPerceptronHeight perc
    in  case pinType of
        InputPin  -> pos - (SDL.V2 (perceptronWidth `div` 2) verticalPos)
        OutputPin -> pos + (SDL.V2 (perceptronWidth `div` 2) verticalPos)
