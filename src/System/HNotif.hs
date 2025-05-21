{-# LANGUAGE OverloadedStrings #-}
module System.HNotif where

import DBus
import DBus.Client

import Control.Monad
import Control.Concurrent
import Data.Maybe (isJust, fromJust)

import System.Exit (exitFailure)
import System.HNotif.Meta

notifyBus :: String
notifyBus = "org.freedesktop.Notifications"

notifyPath :: ObjectPath
notifyPath = objectPath_ "/org/freedesktop/Notifications"

hnotif :: IO ()
hnotif = do
    client <- connectSession
    result <- requestName client (busName_ notifyBus) []

    let e = handleResult result
    when (isJust e) $ do
        putStrLn (fromJust e)
        exitFailure

    export client notifyPath defaultInterface
        { interfaceName = interfaceName_ notifyBus
        , interfaceMethods =
            [ autoMethod "GetCapabilities" capabilities
            , autoMethod "GetServerInformation" serverInformation
            --, autoMethod "Notify" undefined
            --, autoMethod "CloseNotification" undefined
            ]
        }

    putStrLn "listening..."
    forever $ threadDelay 1000000

handleResult :: RequestNameReply -> Maybe String
handleResult NamePrimaryOwner = Nothing
handleResult NameAlreadyOwner = Nothing
handleResult NameInQueue = Just "waiting in queue"
handleResult NameExists = Just "name is already taken, cannot take over service"
handleResult _ = Just "something whent wrong"
