{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameData where
--
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State.Strict (StateT(..), MonadState(..))

import qualified Data.Map
import qualified SDL

import Scene
import Input

newtype Minibrain a = Minibrain (ReaderT Config (StateT GameData IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,
              MonadState GameData)

data Config = Config SDL.Window SDL.Renderer
data GameData = GameData
              { sceneData  :: SceneData
              , cameraData :: CameraData
              , inputData  :: InputData }

data CameraData = CameraData