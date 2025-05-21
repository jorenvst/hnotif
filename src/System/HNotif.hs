{-# LANGUAGE OverloadedStrings #-}
module System.HNotif where

import DBus
import DBus.Client

import Control.Monad
import Control.Concurrent
import Data.Maybe (isJust, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

import System.Exit (exitFailure)
import System.HNotif.Meta
import System.HNotif.Notifications
import Data.IORef (newIORef, readIORef, modifyIORef)
import DBus.Internal.Message

data HNotifConfig = HNotifConfig
    { defaultTimeout :: Timeout
    }

defaultHNotifConfig :: HNotifConfig
defaultHNotifConfig = HNotifConfig
    { defaultTimeout = seconds 1
    }

notifyBus :: String
notifyBus = "org.freedesktop.Notifications"

notifyPath :: ObjectPath
notifyPath = objectPath_ "/org/freedesktop/Notifications"

hnotif :: HNotifConfig -> IO ()
hnotif config = do
    client <- connectSession
    result <- requestName client (busName_ notifyBus) []

    let e = handleResult result
    when (isJust e) $ do
        putStrLn (fromJust e)
        exitFailure

    activeNotifications <- newIORef Map.empty
    export client notifyPath defaultInterface
        { interfaceName = interfaceName_ notifyBus
        , interfaceMethods =
            [ autoMethod "GetCapabilities" capabilities
            , autoMethod "GetServerInformation" serverInformation
            , autoMethod "Notify" (notify client activeNotifications)
            , autoMethod "CloseNotification" (\i -> closeNotification client activeNotifications i *> emit client closeSignal)
            ]
        }
    putStrLn "listening..."
    -- TODO: make sure that loop takes exactly 1 ms by measuring time
    -- TODO: make cleaner
    forever $ do
        modifyIORef activeNotifications (Map.fromList . updateTimeouts config . Map.toList)
        readIORef activeNotifications >>= displayNotifications
        threadDelay 1000

displayNotifications :: Map ID Notification -> IO ()
displayNotifications notifications = mapM_ (\(_,n) -> putStrLn ("[" ++ show (timeout n) ++ "] " ++summary n ++ ": " ++ body n)) (Map.toList notifications)

updateTimeouts :: HNotifConfig -> [ (ID, Notification) ] -> [ (ID, Notification) ]
updateTimeouts _ [] = []
updateTimeouts config ((i,n):ns)
    | isForever $ timeout n = (i,n) : updateTimeouts config ns
    | isDefaultDuration $ timeout n = updateTimeouts config ((i, n { timeout = defaultTimeout config }) : ns)
    | d == 0 = updateTimeouts config ns
    | otherwise = (i, n { timeout = Duration (d - 1) }) : updateTimeouts config ns
    where (Duration d) = timeout n

handleResult :: RequestNameReply -> Maybe String
handleResult NamePrimaryOwner = Nothing
handleResult NameAlreadyOwner = Nothing
handleResult NameInQueue = Just "waiting in queue"
handleResult NameExists = Just "name is already taken, cannot take over service"
handleResult _ = Just "something whent wrong"

closeSignal :: Signal
closeSignal = Signal notifyPath (interfaceName_ notifyBus) "NotificationClosed" Nothing Nothing []
