module System.HNotif.Display.Display where

import System.HNotif.Configuration
import System.HNotif.Types

import DBus.Client

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List ((\\))

import Graphics.UI.Gtk

import Control.Concurrent.STM (TVar, readTVarIO, modifyTVar, atomically, newTVarIO)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

type DisplayState = Map ID Window
type DisplayStateVar = TVar DisplayState

hnotifDisplay :: Client -> HNotifConfig -> HNotifStateVar -> IO ()
hnotifDisplay client config state = do
    _ <- initGUI
    dState <- newTVarIO Map.empty
    _ <- forkIO $ watchIO client config state dState
    mainGUI

-- TODO: not only watch for changed ids but also replaced notifications
watchIO :: Client -> HNotifConfig -> HNotifStateVar -> DisplayStateVar -> IO ()
watchIO client config state dState = forever $ do
    ns <- notifications <$> readTVarIO state
    let k1s = Map.keys ns
    ws <- readTVarIO dState
    let k2s = Map.keys ws

    let newKs = k1s \\ k2s
    newWindows <- Map.fromList . zip newKs <$> mapM (\i -> spawnWindow client config i (ns Map.! i)) newKs
    atomically $ modifyTVar dState $ Map.union newWindows

    let oldKS = k2s \\ k1s
    oldWindows <- Map.fromList . zip oldKS <$> mapM (\i -> widgetHide (ws Map.! i)) oldKS
    atomically $ modifyTVar dState (`Map.difference` oldWindows)

    threadDelay (updateTime config)

spawnWindow :: Client -> HNotifConfig -> ID -> Notification -> IO Window
spawnWindow _ _ i n = do
    win <- windowNew
    set win [ windowTitle := "window for " ++ show i ]
    widgetShowAll win
    return win
