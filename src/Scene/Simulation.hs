{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Scene.Simulation where

--
import qualified SDL
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import Data.Maybe (fromJust, listToMaybe)
import Data.List (find)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Data.Graph.AStar
import qualified Data.HashSet as H

import Types
import Globals
import Utils

data SimulationData = SimulationData