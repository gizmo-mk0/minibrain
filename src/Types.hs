{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State.Strict (StateT(..))

import qualified SDL
import GHC.Word (Word8(..))

newtype Minibrain a = Minibrain (ReaderT Config (StateT Vars IO) a)
    deriving (Functor, Applicative)

data Config = Config
data Vars = Vars

type Color = SDL.V4 Word8

data Scene = Title
           | Briefing
           | Editor
           | Simulation
           | Quit
           deriving (Show, Eq)
