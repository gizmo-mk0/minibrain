{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader)
import Control.Monad.State.Strict (StateT(..), MonadState)
import Control.Exception.Safe (MonadThrow, MonadCatch)

import qualified SDL
import GHC.Word (Word8(..))

newtype Minibrain a = Minibrain (ReaderT Config (StateT Vars IO) a)
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config,
              MonadState Vars, MonadThrow, MonadCatch)

data Config = Config
data Vars = Vars

type Color = SDL.V4 Word8

-- data GameScene       = Menu MenuData
--                      | Briefing BriefingData
--                      | Editor EditorData
--                      | Simulation SimulationData
--                      | Quit
