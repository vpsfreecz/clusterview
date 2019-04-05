module UI.Views.Cluster where

import Brick.Types
import Brick.Main
import UI.Events
import UI.Names
import UI.Types
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Cluster as C

import Lens.Micro

draw :: AppState -> [Widget Name]
draw s = Main.draw s $ C.mkWidget (s^.clusterStateL)

event :: AppState -> BrickEvent Name AppEvent -> NextState
event s e = case e of
    ev -> do ns <- C.handleEvent ev (s^.clusterStateL)
             continue $ s & clusterStateL .~ ns
