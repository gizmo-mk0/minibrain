{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameData where
--
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State.Strict (StateT(..), MonadState(..))

import qualified Data.Map
import qualified SDL
import qualified Graphics.Gloss.Rendering as G

import Scene
import Input
import Types

newtype Minibrain a = Minibrain (ReaderT Config (StateT GameData IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,
              MonadState GameData)

data Config = Config
            { getWindow     :: SDL.Window
            -- , getRenderer   :: SDL.Renderer
            , getWindowSize :: Vector2i
            , getGlossState :: G.State }
            -- , getTexture  :: SDL.Texture }
data GameData = GameData
              { sceneData  :: SceneData
              , cameraData :: CameraData
              , inputData  :: InputData }

data CameraData = CameraData
                { cPosition   :: Vector2f
                , cRotation   :: Float
                , cZoom       :: Float }