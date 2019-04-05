module UI.Widgets.Common where

import qualified Data.Vector as Vector

import Data.Time.LocalTime
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import UI.Events
import UI.Names
import TH (makeSuffixLenses)
import Lens.Micro --((^.))
import qualified Graphics.Vty as V

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Text.Printf

import Data.Prometheus
import System.Remote.Monitoring.Cluster.Types

renderState :: State -> Widget Name
renderState Ok = stateAttr Ok $ str "All good"
renderState (Error msg) = stateAttr (Error msg) $ hBox [ str "Error", str " [", str $ show msg, str "] "]
renderState s = stateAttr s $ str $ show s

stateAttr :: State -> Widget n -> Widget n
stateAttr s = withAttr ((attrName $ "state" ++ show s))

renderClusterState :: (ZonedTime -> String) -> Cluster -> Widget Name
renderClusterState timeFmt Cluster{..} = vBox [
    hBox [ str "Cluster state: ", renderState clusterState ]
  , renderNodes timeFmt clusterNodes
  ]

renderNodes :: (ZonedTime -> String) -> M.Map (String, String) Node -> Widget Name
renderNodes timeFmt = vBox . map (renderNode timeFmt) . M.toList

renderNode :: (ZonedTime -> String) -> ((NodeName, NodeLocation), Node) -> Widget Name
renderNode timeFmt ((name, loc), (Node{..})) = vBox [
    hBox [ withAttr nodeNameAttr $ str name, str ".", str loc, str ": ", renderState nodeState ]
  , renderMonitors timeFmt nodeMonitors
  ]

renderMonitors :: (ZonedTime -> String) -> M.Map NodeFeature Monitor -> Widget Name
renderMonitors timeFmt xs = vBox $ M.elems $ M.mapWithKey (renderMonitor timeFmt) xs

renderMonitor :: (ZonedTime -> String) -> NodeFeature -> Monitor -> Widget Name
renderMonitor timeFmt feat Monitor{..} = vBox $ [
    (str "Monitor ") <+> (str $ show feat)
  , (str "Last attempt: ") <+> (str $ maybe "No data" timeFmt $ monitorLastAttempt)
  , (str "Last success: ") <+> (str $ maybe "No data" timeFmt $ monitorLastValid)
  ] ++ (M.elems $ M.mapWithKey (renderMetric Crit) monitorCrits)
    ++ (M.elems $ M.mapWithKey (renderMetric Warn) monitorWarnings)
  -- node_boot_time_seconds
  -- -}

renderMetric :: State -> MetricId -> Metric -> Widget Name
renderMetric state i@MetricId{..} value = stateAttr state $ hBox [
    padLeft Max $ str $ B.unpack $ prettyID i
  , str ": "
  , renderMetricValue ("_bytes" `B.isSuffixOf` name) value
  ]

renderMetricValue :: Bool -> Metric -> Widget Name
renderMetricValue True  (Counter x) = prettyBytes x
renderMetricValue True  (Gauge x) = prettyBytes x
renderMetricValue False x = renderMetricValue' x

renderMetricValue' :: Metric -> Widget Name
renderMetricValue' (Counter x) = dbl x
renderMetricValue' (Gauge x) = dbl x

units :: [String]
units = [ "B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB" ]

prettyBytes :: Double -> Widget Name
prettyBytes n = hBox [ dbl x, str $ units !! e ]
  where
    e = min (length units - 1) (round $ logBase 10 n / 3)
    x = n / 1000^e

dbl :: Double -> Widget n
dbl = str . printf "%.2f"

nodeNameAttr :: AttrName
nodeNameAttr = "nodename"

attrs :: [(AttrName, V.Attr)]
attrs = [
    (nodeNameAttr, fg V.green `V.withStyle` V.bold)
  , ("stateOk", fg V.green)
  , ("stateUnknown", fg V.cyan)
  , ("stateWarn", fg V.yellow)
  , ("stateCrit", fg V.red)
  , ("stateFail", fg V.red)
  , ("stateError", fg V.red)
  ]
