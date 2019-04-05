{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Remote.Monitoring.Cluster

import Lens.Micro --((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Data.Default
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import qualified Data.Map as M

import Text.Time.Pretty

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Prometheus
import qualified Data.ByteString.Char8 as B

import Brick
import Brick.BChan
import Brick.Types
import Brick.Forms
import Brick.Main
import qualified Brick.Widgets.Border as B
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , hBox
  , withAttr
  , padLeft
  , padRight
  )
import Brick.Util (fg, on)

import UI.Events
import UI.Types
import UI.Names
import qualified UI.Views.Cluster as CV
import qualified UI.Views.Main as MV

import qualified UI.Widgets.Cluster as CW
import qualified UI.Widgets.Common as CommW

drawUI :: AppState -> [Widget Name]
drawUI s = case s^.showHelpL of
  True -> [ renderDialog (dialog (Just "Help") Nothing 50) drawHelp ]
  False ->
    case s^.viewL of
      ClusterView -> CV.draw s

drawHelp :: Widget n
drawHelp = vCenter $ vBox [ hCenter $ str "Help"
                          , hCenter $ str "Press 'q' or 'Esc' to exit."
                          ]

appEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent (V.EvKey (V.KChar 'q') []) -> halt st
        --VtyEvent _ -> continue $ st & stLastBrickEvent .~ (Just e)
        --AppEvent Counter -> continue $ st & stCounter %~ (+1)
        --                                  & stLastBrickEvent .~ (Just e)
        AppEvent (UpdateCluster c) -> do
          continue $ st & clusterL .~ c
                        & clusterStateL . CW.clusterL .~ c
        AppEvent (Clock t) -> do
          let tf = prettyTimeAuto (zonedTimeToUTC t) . zonedTimeToUTC
          continue $ st & timeL .~ t
                        & timeFnL .~ tf
                        & clusterStateL . CW.timeFnL .~ tf
        _ -> continue st

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr $ concat
  [ CommW.attrs
  , CW.attrs
  , MV.attrs
  ]

theApp = App {
    appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

main :: IO ()
main = do
  now <- getCurrentTime
  znow <- getZonedTime
  let tf = prettyTimeAuto now . zonedTimeToUTC
  let iniState = AppState {
          _view = ClusterView
        , _cluster = newClusterState
        , _clusterState = CW.mkState newClusterState tf
        , _timeFn = tf
        , _time = znow
        , _showHelp = False
        }

  q <- atomically $ newTBQueue 10
  b <- newBChan 10 -- bricks BChan which is just hidden TBQueue

  async $ runClusterfuck q
  async $ forever $ (atomically $ readTBQueue q) >>= writeBChan b . UpdateCluster

  -- clock
  void . async . forever $ getZonedTime >>= writeBChan b . Clock >> threadDelay 1000000

  void $ customMain (V.mkVty V.defaultConfig) (Just b) theApp iniState
