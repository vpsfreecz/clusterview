module UI.Views.Main where

import Data.Time.Format

import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Types
import Brick.Main
import qualified Graphics.Vty as V
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Widgets.Cluster as C
import qualified UI.Widgets.Common as Common
import Lens.Micro

import System.Remote.Monitoring.Cluster.Types

draw :: AppState -> Widget Name -> [Widget Name]
draw s widget = [ui]
  where
    ui = vBox widgets
    activeViewWidget = str $ show (s^.viewL)
    view = hBorder <=> (str "       " <+> hCenter activeViewWidget <+> str "?: Help")
    tabs view = (mkTab 1 ClusterView view "Cluster") <+> (mkTab 2 NodeView view "Node")
    mkTab n forView view pretty =
      let active = if forView == view
                   then withAttr selectedTabAttr
                   else id
      in active (str "[" <+> str (show n) <+> str "] ") <+> withAttr tabNameAttr (str pretty) <+> str " "

    cState = hCenter $ str "Cluster state: " <+> Common.renderState (clusterState $ s^.clusterL)

    widgets = [
        (tabs $ s^.viewL) <+> cState <+> (str $ formatTime defaultTimeLocale "%F %X" $ s^.timeL)
      , hBorder
      , hCenter widget
      , view
      ]

{-
event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do
      case s^.viewL of
        ClusterView -> do ns' <- C.handleEvent ev (s^.clusterStateL)
                          continue $ s & clusterStateL .~ ns'
-}

selectedTabAttr :: AttrName
selectedTabAttr = "selectedTab"

tabNameAttr :: AttrName
tabNameAttr = "tabName"

attrs :: [(AttrName, V.Attr)]
attrs = [ (tabNameAttr, fg V.yellow)
        , (selectedTabAttr, fg V.white `V.withStyle` V.bold)
        ]

