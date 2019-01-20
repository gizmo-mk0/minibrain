module Scene
    ( Scene (..)
    , SceneData (..)
    , TitleData (..)
    , BriefingData (..)
    , SimulationData (..)
    , module Scene.Editor )
    where

--
import qualified Data.IntMap as Map

import Scene.Editor

data SceneData = SceneData
               { currentScene   :: Scene
               , titleData      :: TitleData
               , briefingData   :: BriefingData
               , editorData     :: EditorData
               , simulationData :: SimulationData }

data Scene = Title
           | Briefing
           | Editor
           | Simulation
           | Quit
           deriving (Eq)

data TitleData = TitleData
data BriefingData = BriefingData
data SimulationData = SimulationData
