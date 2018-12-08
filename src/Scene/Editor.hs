module Scene.Editor where

--

import qualified Data.IntMap as Map

data EditorData = EditorData
                { perceptrons :: Map.IntMap Perceptron
                , connectors  :: Map.IntMap Connector}

data Perceptron = Perceptron
                { pins :: Map.IntMap Pin }
data Pin = Pin
         { getType  :: PinType
         , parentId :: Int }
data PinType = InputPin | OutputPin deriving (Eq)
data Connector = Connector Int Int

defaultEditorData :: EditorData
defaultEditorData = EditorData Map.empty Map.empty
