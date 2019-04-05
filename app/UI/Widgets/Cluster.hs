module UI.Widgets.Cluster where

import qualified Data.Vector as Vector

import Data.Time.LocalTime
import Brick hiding (attrName)
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import UI.Events
import UI.Names
import TH (makeSuffixLenses)
import Lens.Micro --((^.))
import qualified Graphics.Vty as V

import qualified System.Remote.Monitoring.Cluster.Types as C

import qualified UI.Widgets.Common as CW

--import UI.Widgets.Build (statusAttr)

data ClusterState = ClusterState
  {
    _cluster :: C.Cluster
  , _timeFn :: ZonedTime -> String
  -- _list :: List Name B.Build
  --, _ppConf :: P.PPConf
  }

makeSuffixLenses ''ClusterState

mkState :: C.Cluster -> (ZonedTime -> String) -> ClusterState
mkState c tf = ClusterState
    { _cluster = c
    , _timeFn = tf
    }
  --{ _list = list BuildList (Vector.empty) 1
  --, _ppConf = pp }

mkWidget :: ClusterState -> Widget Name
mkWidget s = vBox [
      CW.renderNodes (s^.timeFnL) (C.clusterNodes $ s^.clusterL)
    ]
    where
        --label = str "Build " <+> cur <+> str " of " <+> total
        --cur = case s^.listL.listSelectedL of
        --        Nothing -> str "-"
        --        Just i -> str (show (i + 1))
        --total = str $ show $ Vector.length $ s^.listL.listElementsL
        box = borderWithLabel (str "lol") $
              str "Nada"
              --renderList (drawBuild timeFn) True $ s^.listL
        ui = vCenter $ box
        --timeFn = P.timeRenderer $ s^.ppConfL

attrName :: AttrName
attrName = "build"

projectNameAttr :: AttrName
projectNameAttr = attrName <> "projectName"

timeAgoAttr :: AttrName
timeAgoAttr = attrName <> "timeAgo"

selectedAttr :: AttrName
selectedAttr = listSelectedAttr <> "selected"

attrs :: [(AttrName, V.Attr)]
attrs = [ (attrName, fg V.green)
        , (projectNameAttr, fg V.green `V.withStyle` V.bold)
        , (timeAgoAttr, fg V.yellow)
        , (selectedAttr, fg V.cyan `V.withStyle` V.bold)
        ]

handleEvent :: BrickEvent Name AppEvent -> ClusterState -> EventM Name ClusterState
handleEvent e@(VtyEvent ev) s = case e of
  _ -> do
    --l' <- handleListEvent ev (s^.listL)
    --return $ s & listL .~ l'
    return s
