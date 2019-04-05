{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Remote.Monitoring.Cluster (
    runClusterfuck
  , module CF
  ) where

import System.Remote.Monitoring.Cluster.Types as CF

import Data.Default
import Data.Time.LocalTime
import Data.Prometheus

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Text.Regex.Posix

import System.Timeout
import qualified Network.HTTP.Client as HTTP

base = "base48.cz"
stg = "stg.vpsfree.cz"

nodes = [
    NodeConfig [Prometheus] "devnode1" base ["172.17..."]
  , NodeConfig [Prometheus] "fox"      base ["172.0.0.1"]
  , NodeConfig [Prometheus] "pxe"      base []
  , NodeConfig [Prometheus] "node1"    stg  []
  --, NodeConfig [Prometheus] "node1"    base  []
  ]

kb = (*1024)
mb = (*1024**2)
gb = (*1024**3)

alert metric warnOp critOp = (metric, Nothing, Just warnOp, Just critOp)
alert' metric subFilter warnOp critOp = (metric, Just subFilter, Just warnOp, Just critOp)
zero metric  = (metric, Nothing, Nothing, Just (==0))
nonzero metric  = (metric, Nothing, Nothing, Just (>1))

byLabelRe :: String -> B.ByteString -> M.Map MetricId a -> M.Map MetricId a
byLabelRe metric re = byLabel' metric (\x -> (B.unpack x) =~ re)

alerts = [
    alert "node_load1" (>1) (>=100)
  , alert' "node_filesystem_avail_bytes" (byLabel "mountpoint" "/") (< gb 30) (< gb 1)
  , nonzero "node_edac_"
  , alert "node_hwmon_temp_celsius" (>60) (>90)
  , zero "node_scrape_collector_success"
  , nonzero "node_textfile_scrape_error"
  , nonzero "node_filesystem_device_error"
  , alert "node_entropy_available_bits" (< 1000) (<500)
  , alert "node_memory_MemAvailable_bytes" (< gb 5) (< gb 2)
  , alert' "node_network_up" (byLabelRe "interface" "teng(0|1)") (==0) (==0)
  , alert' "node_network_carrier_changes_total" (byLabelRe "interface" "teng(0|1)") (>0) (>0)
  ]

gOrC (Gauge x) = x
gOrC (Counter x) = x
gOrC _ = error "Not a Gauge or Counter"

filterOp _    _          Nothing  _  = M.empty
filterOp name subFilter (Just op) ms = M.filter (\x -> op $ gOrC x) (maybe id id subFilter $ filterMetrics name ms)
warnMetrics ms = M.unions $ map (\(name, sub, warnOp, _) -> filterOp name sub warnOp ms) alerts
critMetrics ms = M.unions $ map (\(name, sub, _, critOp) -> filterOp name sub critOp ms) alerts

runClusterfuck q = do
  cst <- atomically $ newTVar newClusterState
  nodeStates <- M.fromList <$> forM nodes runNode
  atomically $ modifyTVar cst $ \x -> x { clusterNodes = nodeStates }

  ist <- atomically $ newTVar $ IState M.empty
  mons <- M.fromList <$> forM nodes runNodeMonitors
  atomically $ modifyTVar ist $ \x -> x { iNodes = mons }

  async $ forever $ do
    ms <- atomically $ do
      is <- readTVar ist
      nodeSnap <- forM (M.toList $ iNodes is) $ \((name, loc), n) -> do
        monSnap <- forM (nodeStateMonitors n) $ \x -> do
          mon <- readTVar x

          -- # HELP node_scrape_collector_duration_seconds node_exporter: Duration of a collector scrape.
          -- # TYPE node_uname_info gauge
          -- node_uname_info{domainname="(none)",machine="x86_64",nodename="node1.stg",release="4.19.26",sysname="Linux",version="#1-NixOS SMP Wed Feb 27 09:09:03 UTC 2019"} 1
          -- # HELP node_disk_io_now The number of I/Os currently in progress.
          -- node_disk_io_now
          --
          -- TIME OFF?
          -- node_time_seconds
          --
          -- MEMINFO
          --
          -- node_memory_MemTotal_bytes 2.70455742464e+11
          -- node_memory_MemFree_bytes 3.948146688e+09
          --
          -- SwapFree
          -- SwapTotal
          -- 
          -- Slab
          -- Shmem
          -- Cached
          -- SwapCached
          -- node_memory_Buffers_bytes
          -- -> mem_free_no_cache = memFree + swapCached + Buffers
          -- Committed_AS
          --
          --
          -- arc_c_max
          -- arc_c
          -- arc_size        arc_data_size?
          -- arc_hdr_size
          -- arc_l_hdr_size
          -- arc_l2_hdr_size
          -- arc_mfu_size
          -- arc_mfu_ghost_size
          -- arc_mru_size
          -- arc_mru_ghost_size
          -- arc_meta_limit
          -- arc_meta_used
          -- arc_meta_max
          --
          -- arc_hits
          -- arc_misses
          --
          -- arc_l2_hits
          -- arc_l2_misses
          --
          --
          -- NETINFO
          --
          -- node_network_up
          -- node_nfs_connections_total
          -- node_nfs_packets_total{protocol="tcp"} 0
          -- node_nfs_packets_total{protocol="udp"} 0
          -- node_nfs_rpc_retransmissions_total
          -- node_nfsd_connections_total
          -- node_nfsd_disk_bytes_read_total
          -- node_nfsd_disk_bytes_written_total
          --
          -- node_netstat_Ip_Forwarding
          -- node_netstat_Icmp_InMsgs
          -- node_netstat_Icmp_OutMsgs
          -- node_netstat_Icmp6_InMsgs
          -- node_netstat_Ip6_InOctets 1.035935968e+10
          -- node_netstat_IpExt_OutOctets 3.51639568797e+11
          --
          -- node_network_carrier_changes_total
          -- node_network_carrier_up_changes_total
          -- node_network_carrier_down_changes_total
          --
          -- node_network_receive_bytes_total
          -- node_network_receive_drop_total
          -- node_network_receive_drop_total{device="teng0"} 1059
          -- node_network_receive_errs_total
          -- node_network_receive_multicast_total
          -- node_network_receive_packets_total
          -- node_network_speed_bytes
          -- node_network_speed_bytes{interface="teng0"} 1.25e+09
          -- node_network_transmit_bytes_total
          -- node_network_transmit_packets_total
          --
          -- node_netstat_Tcp_ActiveOpens
          -- node_netstat_Tcp_CurrEstab
          --
          -- node_netstat_Udp_InDatagrams 260420
          -- node_netstat_Udp_OutDatagrams 260666
          --
          -- # HELP node_network_address_assign_type address_assign_type value of /sys/class/net/<iface>.
          -- node_network_address_assign_type
          --
          -- node_network_carrier
          -- node_network_carrier{interface="teng0"} 1
          --
          --
          -- SERVICES
          -- node_service_desired_state
          -- node_service_state
          -- node_service_state_last_change_timestamp_seconds
          --
          let ms = monData mon
              crits = critMetrics ms
              -- ideally substract ms - crits here
              --
              warns = warnMetrics (ms `M.difference` crits)

          let allStates = [ if null warns then Ok else Warn
                          , if null crits then Ok else Crit
                          , monState mon
                          ]
          return $ (monFeature mon, Monitor {
              monitorState = worst allStates
            , monitorData  = monData mon
            , monitorLastAttempt  = monLastAttempt mon
            , monitorLastValid  = monLastValid mon
            , monitorWarnings = warns
            , monitorCrits    = crits
            })

        let newState = worst $ map (monitorState . snd) monSnap

        return $ ((name, loc), Node {
            nodeState = newState
          , nodeName = name
          , nodeLocation = loc
          , nodeMetrics = M.fromList []
          , nodeMonitors = M.fromList monSnap })


      let newClusterState = worst $ map (nodeState . snd) nodeSnap
          newCluster = Cluster newClusterState (M.fromList nodeSnap)
      writeTBQueue q newCluster
      writeTVar cst $ newCluster
          --modifyTVar cst
          --  monData mon
    --print ms
    threadDelay 1000000

  {-
  forever $ do
    atomically $ readTVar cst >>= writeTBQueue q
    threadDelay 1000000
  -}

-- external iface 
-- make it 
runNode nc@NodeConfig{..} = do
  return ((nodeConfigName, nodeConfigLocation), Node {
      nodeState = Unknown
    , nodeName = nodeConfigName
    , nodeLocation = nodeConfigLocation
    , nodeMonitors = M.fromList []
    , nodeMetrics = M.fromList []
    })

mkAddr NodeConfig{..} = concat [ nodeConfigName, ".", nodeConfigLocation ]

--runNodeMonitors :: NodeConfig -> IO [TVar (Monitor PromMetrics)]
runNodeMonitors n@NodeConfig{..} = do
  mons <- mapM (runMonitor (mkAddr n)) nodeConfigFeatures
  return ((nodeConfigName, nodeConfigLocation), NodeState mons)

runMonitor addr feat = do
  ps <- atomically $ newTVar $ newMonitorState

  --mq <- atomically $ newTBQueue 10
  pt <- case feat of
    Prometheus -> async $ runPrometheus addr ps
    x -> error $ "Don't know how to run" ++ show x
  atomically $ modifyTVar ps $ \x -> x { monThread = Just pt
                                       , monFeature = feat }
                                       --, monUpdateQueue = mq }
  return ps

runPrometheus addr s = forever $ do
  let settings = HTTP.defaultManagerSettings {
        HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 1000000}
  manager <- HTTP.newManager settings
  request <- HTTP.parseRequest $ promHTTPAddr addr
  response <- liftIO (tryAny (HTTP.httpLbs request manager))
  t <- getZonedTime
  atomically $ modifyTVar s $ \x -> x { monLastAttempt = Just t }

  case response of
    Left ex -> atomically $ modifyTVar s $ \x -> x { monData = def
                                                   , monErrors = (monErrors x) + 1
                                                   , monState = Error $ "HTTP exception: " ++ (show ex) }
    Right r ->
      case parseProm (BL.toStrict $ HTTP.responseBody r) of
        Right result -> do
          t <- getZonedTime
          atomically $ modifyTVar s $ \x -> x { monData = result
                                              , monState = Ok
                                              , monLastValid = Just t }
        Left err -> do
          atomically $ modifyTVar s $ \x -> x { monData = def
                                              , monErrors = (monErrors x) + 1
                                              , monState = Error $ "Parsing error: " ++ err }


  loopTime <- fmap monLoopTime <$> atomically $ readTVar s
  {-
  atomically $ do
    m <- readTVar s
    writeTBQueue (monUpdateQueue m) Ok
  -}

  threadDelay (loopTime * 1000000)
  where
    promHTTPAddr x = "http://" ++  x ++ ":9100/metrics"
