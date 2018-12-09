module Scene.Editor where

--
import qualified SDL
import qualified Data.IntMap as Map
import Foreign.C.Types (CInt)

import Types
import Globals

data EditorData = EditorData
                { perceptrons :: Map.IntMap Perceptron
                , connectors  :: Map.IntMap Connector}

data Perceptron = Perceptron
                { position :: Vector2
                , pins     :: Map.IntMap Pin }
data Pin = Pin
         { getType  :: PinType
         , parentId :: Int }
data PinType = InputPin | OutputPin deriving (Eq)
data Connector = Connector Int Int

defaultEditorData :: EditorData
defaultEditorData = EditorData (Map.singleton 1 (Perceptron (SDL.V2 300 400) (Map.fromList [(0, (Pin InputPin 1))
                                                                                           ,(1, (Pin OutputPin 1))
                                                                                           ,(2, (Pin InputPin 1))]))) Map.empty

getPerceptronHeight :: Perceptron -> CInt
getPerceptronHeight p =
    perceptronBodyRoundness * 2 + moduleCount * perceptronModuleHeight
    where
    moduleCount    = fromIntegral $ max inputPinCount outputPinCount
    inputPinCount  = Map.size $ Map.filter (\(Pin t _) -> t == InputPin)  (pins p)
    outputPinCount = Map.size $ Map.filter (\(Pin t _) -> t == OutputPin) (pins p)
