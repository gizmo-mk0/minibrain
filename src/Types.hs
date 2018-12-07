{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State.Strict (StateT(..), MonadState(..))

import qualified Data.Map

import qualified SDL
import GHC.Word (Word8(..))

import Input

type Color = SDL.V4 Word8

newtype Minibrain a = Minibrain (ReaderT Config (StateT GameData IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,
              MonadState GameData)

data Config = Config SDL.Window SDL.Renderer
data GameData = GameData
              { sceneData  :: SceneData
              , cameraData :: CameraData
              , inputData  :: InputData }

data SceneData = SceneData
               { currentScene   :: Scene
               , titleData      :: TitleData
               , briefingDara   :: BriefingData
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
data EditorData = EditorData
data SimulationData = SimulationData

data CameraData = CameraData
