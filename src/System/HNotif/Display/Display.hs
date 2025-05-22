module System.HNotif.Display.Display where

import System.HNotif.Configuration
import System.HNotif.Types

import DBus.Client

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import Control.Concurrent.STM (TVar, readTVarIO, atomically, newTVarIO, writeTVar)
import Control.Monad (forever, foldM, void, forM)
import Control.Concurrent (threadDelay, forkIO)

type DisplayState = Map ID Window
type DisplayStateVar = TVar DisplayState

hnotifDisplay :: Client -> HNotifConfig -> HNotifStateVar -> IO ()
hnotifDisplay client config state = do
    void initGUI
    dState <- newTVarIO Map.empty
    _ <- forkIO $ stepIO client config state dState
    mainGUI

-- TODO: not only watch for changed ids but also replaced notifications
-- TODO: when window is closed, update the global state and send signal
stepIO :: Client -> HNotifConfig -> HNotifStateVar -> DisplayStateVar -> IO ()
stepIO client config s ds = forever $ do
    s' <- readTVarIO s
    ds' <- readTVarIO ds
    uds <- step client config s' ds'
    atomically $ writeTVar ds uds
    threadDelay (updateTime config)

step :: Client -> HNotifConfig -> HNotifState -> DisplayState -> IO DisplayState
step _ = updateWindows

updateWindows :: HNotifConfig -> HNotifState -> DisplayState -> IO DisplayState
updateWindows config s ds = do
    eds <- foldr Map.delete ds <$> mapM hide (expiredNotifications s ds)
    updated <- foldM create eds (newNotifications s ds)
    applyOffsets config s updated >> mapM_ widgetShow updated >> return updated
    where
        ns = notifications s
        hide i = widgetHide (ds Map.! i) >> return i
        create acc i = window config (i, ns Map.! i) >>= \(_,w) -> return $ Map.insert i w acc

newNotifications :: HNotifState -> DisplayState -> [ ID ]
newNotifications s ds = Map.keys (Map.filterWithKey (\i _ -> not . Map.member i $ ds) (notifications s))

expiredNotifications :: HNotifState -> DisplayState -> [ ID ]
expiredNotifications s ds = Map.keys (Map.filterWithKey (\i _ -> not . Map.member i $ notifications s) ds)

-- TODO: support for multiple monitors
-- TODO: get sizes from config
window :: HNotifConfig -> (ID, Notification) -> IO (ID, Window)
window config (i,n) = do
    win <- windowNewPopup
    set win
        [ windowTitle := "hnotif"
        , windowAcceptFocus := False
        ]
    windowSetDefaultSize win 200 100
    return (i, win)

applyOffsets :: HNotifConfig -> HNotifState -> DisplayState -> IO ()
applyOffsets config s ds = mapM_ (\(index,(i,_)) -> applyOffset config index (ds Map.! i)) (zip [0..] ns)
    where ns = sortBy (\(_,n1) (_,n2) -> compare (receiveTime n1) (receiveTime n2)) . Map.toList $ notifications s

applyOffset :: HNotifConfig -> Int -> Window -> IO Window
applyOffset config index w = do
    let (x,y) = spawn config (getSize w) index
    windowMove w x y
    return w

-- TODO
getSize :: Window -> NotificationSize
getSize _ = (200, 100)
