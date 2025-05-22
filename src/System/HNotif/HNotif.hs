module System.HNotif.HNotif where

import System.HNotif.Configuration
import System.HNotif.Types
import System.HNotif.DBus.DBus

import Control.Concurrent.STM
import Control.Monad (forever)

hnotif :: HNotifConfig -> IO ()
hnotif config = do
    state <- newTVarIO initialState
    hnotifDBus config state
    putStrLn "listening..."
    forever $ putStr ""
