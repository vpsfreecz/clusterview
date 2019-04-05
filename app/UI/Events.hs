module UI.Events where

import Data.Time.LocalTime

import System.Remote.Monitoring.Cluster.Types

data AppEvent = UpdateCluster Cluster
              | Clock ZonedTime
              deriving (Show, Eq, Ord)

