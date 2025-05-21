module System.HNotif.HNotif where

import System.HNotif.Configuration
import System.HNotif.Types
import System.HNotif.DBus.DBus
import System.HNotif.Display

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever)

hnotif :: HNotifConfig -> IO ()
hnotif config = do
    state <- newTVarIO initialState
    _ <- forkIO $ hnotifDBus config state
    _ <- forkIO $ hnotifDisplay config state
    putStrLn "listening..."
    forever $ putStr ""
