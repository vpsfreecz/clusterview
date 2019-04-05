
module UI.Types where

import TH (makeSuffixLenses)
import UI.Names
import UI.Widgets.Cluster
import Brick
import Control.Monad.Identity
import Control.Concurrent.STM

import Data.Time

import System.Remote.Monitoring.Cluster.Types

data View =
    ClusterView
  | NodeView
  | MetricsView
  deriving (Show, Eq, Ord)

clusterScroll :: ViewportScroll Name
clusterScroll = viewportScroll ClusterVP

data AppState = AppState {
    _view :: View
  , _cluster :: Cluster
  , _clusterState :: ClusterState
  , _timeFn :: ZonedTime -> String
  , _time :: ZonedTime
  , _showHelp :: Bool
  , _nodeState :: Bool -- NodesState
  }

type NextState = EventM Name (Next AppState)

makeSuffixLenses ''AppState
