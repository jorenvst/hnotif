{-# LANGUAGE OverloadedStrings #-}
module System.HNotif.DBus.DBus where

import System.HNotif.Configuration
import System.HNotif.Types
import System.HNotif.Notification
import System.HNotif.DBus.Meta

import DBus.Client
import System.HNotif.DBus.Notification

import qualified Data.Map as Map

import System.Exit (exitFailure)
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)


hnotifDBus :: HNotifConfig -> HNotifStateVar -> IO ()
hnotifDBus config state = do
    client <- connectSession
    requestName client "org.freedesktop.Notifications" [] >>= handleResponse
    export client "/org/freedesktop/Notifications" defaultInterface
        { interfaceName = "org.freedesktop.Notifications"
        , interfaceMethods =
            [ autoMethod "GetCapabilities" capabilities
            , autoMethod "GetServerInformation" serverInformation
            , autoMethod "Notify" $ handleNotification config state
            , autoMethod "CloseNotification" $ handleCloseNotification client config state
            ]
        }
    forever $ do
        updateStateIO client config state
        threadDelay (updateTime config)

-- TODO: make sure main thread closes when this fails
handleResponse :: RequestNameReply -> IO ()
handleResponse NamePrimaryOwner = return ()
handleResponse NameAlreadyOwner = return ()
handleResponse _ = putStrLn "failed to start dbus service" >> exitFailure

updateStateIO :: Client -> HNotifConfig -> HNotifStateVar -> IO ()
updateStateIO client config state = do
    s <- readTVarIO state >>= updateState client config
    atomically $ writeTVar state s

updateState :: Client -> HNotifConfig -> HNotifState -> IO HNotifState
updateState client _ state = do
    let (ids, ns) = unzip . Map.toList . notifications $ state
    newNs <- filterM (fmap not . hasExpired) ns
    let newMap = Map.fromList . zip ids $ newNs
    -- send a signal for each removed notification
    let removed = Map.keys . Map.difference (notifications state) $ newMap
    mapM_ (\i -> emit client $ closeSignal i Expired) removed
    print newNs
    print removed
    return state { notifications = newMap }

