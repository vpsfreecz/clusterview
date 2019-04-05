{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module System.Remote.Monitoring.Cluster.Types where

import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Monad.Identity

import Data.Default
import Data.Prometheus
import Data.Prometheus.Types
import Data.Time
import qualified Data.Map as M
import qualified Data.Set as S

-- internal
data MonitorState a = MonitorState {
    monThread :: Maybe (Async ())
  , monState :: State
  , monData :: a
  , monErrors :: Int
  , monFeature :: NodeFeature
--  , monUpdateQueue :: TBQueue State
  , monLastAttempt :: Maybe ZonedTime
  , monLastValid   :: Maybe ZonedTime
  , monLoopTime :: Int -- Seconds
  }
  deriving (Show)

--type PromMonitor = Monitor PromMetrics
--type PingMonitor = Monitor Int

newMonitorState :: Default a => MonitorState a
newMonitorState = MonitorState {
    monThread = Nothing
  , monState = Unknown
  , monData = def
  , monLastAttempt = Nothing
  , monLastValid = Nothing
  , monLoopTime = 10
  }

--data Monitors = Prom PromMonitor | Ping PingMonitor
--  deriving (Show)

data Monitor = Monitor {
    monitorState :: State
  , monitorData :: PromMetrics
  , monitorLastAttempt :: Maybe ZonedTime
  , monitorLastValid   :: Maybe ZonedTime
  , monitorWarnings :: PromMetrics
  , monitorCrits    :: PromMetrics
  } deriving (Eq, Ord, Show)

data State =
    Ok
  | Warn
  | Crit
  | Fail
  | Unknown
  | Error String
  deriving (Eq, Ord, Show)

badness Ok = 0
badness Warn = 1
badness Crit = 9
badness Fail = 10
badness Unknown = 99
badness (Error _) = 100

worst :: [State] -> State
worst [] = Unknown
worst x = snd . maximum . map (\x -> (badness x, x)) $ x


type NodeName = String
type NodeLocation = String

data Node = Node {
    nodeState :: State
  , nodeName :: NodeName
  , nodeLocation :: NodeLocation
  , nodeMetrics :: PromMetrics
  , nodeMonitors :: M.Map NodeFeature Monitor
  } deriving (Eq, Show, Ord)

data Cluster = Cluster {
    clusterState :: State
  , clusterNodes :: M.Map (NodeName, NodeLocation) Node
  } deriving (Eq, Show, Ord)

newClusterState = Cluster Unknown M.empty
data NodeFeature = Prometheus | ZRE
  deriving (Show, Eq, Ord)

data NodeConfig = NodeConfig {
    nodeConfigFeatures :: [NodeFeature]
  , nodeConfigName :: NodeName
  , nodeConfigLocation :: NodeLocation
  , nodeConfigAddrs :: [String]
  } deriving (Show)

data IState = IState {
    iNodes :: M.Map (NodeName, NodeLocation) NodeState
  }

data NodeState = NodeState {
    nodeStateMonitors :: [TVar (MonitorState PromMetrics)]
  }

-- bugmenot

instance Show a => Show (TVar a) where
  show = pure "TVar"

instance Show a => Show (TBQueue a) where
  show = pure "TBQueue"

instance Show a => Show (Async a) where
  show = pure "Async"

instance Eq ZonedTime where
  ZonedTime t1 tz1 == ZonedTime t2 tz2 = t1 == t2 && tz1 == tz2

instance Ord ZonedTime where
    compare a b = compare (f a) (f b)
        where f t = (zonedTimeToUTC t, zonedTimeZone t)
