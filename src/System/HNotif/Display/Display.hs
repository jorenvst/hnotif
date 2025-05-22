module System.HNotif.Display.Display where

import System.HNotif.Configuration
import System.HNotif.Types

import DBus.Client

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import Control.Concurrent.STM (TVar, readTVarIO, atomically, newTVarIO, writeTVar)
import Control.Monad (forever, foldM, void, foldM_)
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
    applyOffsets config s updated >> mapM_ widgetShowAll updated >> return updated
    where
        ns = notifications s
        hide i = widgetHideAll (ds Map.! i) >> return i
        create acc i = window config (i, ns Map.! i) >>= \(_,w) -> return $ Map.insert i w acc

newNotifications :: HNotifState -> DisplayState -> [ ID ]
newNotifications s ds = Map.keys (Map.filterWithKey (\i _ -> not . Map.member i $ ds) (notifications s))

expiredNotifications :: HNotifState -> DisplayState -> [ ID ]
expiredNotifications s ds = Map.keys (Map.filterWithKey (\i _ -> not . Map.member i $ notifications s) ds)

-- TODO: support for multiple monitors
window :: HNotifConfig -> (ID, Notification) -> IO (ID, Window)
window config (i,n) = do
    w <- windowNewPopup

    -- TODO: extract notification view structure to config
    vbox <- vBoxNew False 10

    title <- labelNew (Nothing :: Maybe String)
    labelSetMarkup title ("<b>" ++ summary n ++ "</b>")
    align1 <- alignmentNew 0 0.5 0 0
    containerAdd align1 title

    b <- labelNew $ Just (body n)
    labelSetLineWrap b True
    align2 <- alignmentNew 0 0.5 0 0
    containerAdd align2 b

    set w
        [ windowTitle := "hnotif"
        , windowAcceptFocus := False
        , containerChild := vbox
        , containerBorderWidth := 10
        ]

    boxPackStart vbox align1 PackGrow 0
    boxPackStart vbox align2 PackGrow 0

    let (dx,dy) = defaultSize config
    windowSetDefaultSize w dx dy
    return (i, w)

applyOffsets :: HNotifConfig -> HNotifState -> DisplayState -> IO ()
applyOffsets config s ds = foldM_ (\(prev,l) (index,(i,_)) -> do
        w <- applyOffset config index prev (ds Map.! i)
        (sx,sy) <- windowGetSize w
        (px,py) <- windowGetPosition w
        return (Just (sx + px, sy + py), w : l)
    ) (Nothing, []) (zip [0..] ns)
    where ns = sortBy (\(_,n1) (_,n2) -> compare (receiveTime n1) (receiveTime n2)) . Map.toList $ notifications s

applyOffset :: HNotifConfig -> Int -> Maybe (Int, Int) -> Window -> IO Window
applyOffset config index prevN w = do
    wSize <- windowGetSize w
    let (x,y) = spawn config index prevN wSize
    windowMove w x y
    return w

