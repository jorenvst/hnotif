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
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef, IORef)
import DBus.Internal.Message
import Data.Word (Word32)

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
            , autoMethod "Notify" (notify activeNotifications)
            , autoMethod "CloseNotification" (\i -> closeNotification activeNotifications i *> emit client (closeSignal i CloseCalled))
            ]
        }
    putStrLn "listening..."
    -- TODO: make sure that loop takes exactly 1 ms by measuring time
    forever $ do
        removeExpired client config activeNotifications
        readIORef activeNotifications >>= displayNotifications
        threadDelay 1000

removeExpired :: Client -> HNotifConfig -> IORef (Map ID Notification) -> IO ()
removeExpired client config ref = do
    notifications <- readIORef ref
    let (nfs, rms) = updateTimeouts config notifications
    writeIORef ref nfs
    mapM_ (\i -> emit client (closeSignal i Expired)) rms

displayNotifications :: Map ID Notification -> IO ()
displayNotifications notifications = mapM_ (\(_,n) -> putStrLn ("[" ++ show (timeout n) ++ "] " ++summary n ++ ": " ++ body n)) (Map.toList notifications)

updateTimeouts :: HNotifConfig -> Map ID Notification -> (Map ID Notification, [ ID ])
updateTimeouts config = Map.foldrWithKey (updateTimeout config) (Map.empty, [])

updateTimeout :: HNotifConfig -> ID -> Notification -> (Map ID Notification, [ ID ]) -> (Map ID Notification, [ ID ])
updateTimeout config i n (acc, rms)
    | isForever $ timeout n = (Map.insert i n acc, rms)
    | isDefaultDuration $ timeout n = (Map.insert i (n { timeout = defaultTimeout config }) acc, rms)
    | d == 0 = (acc, i : rms)
    | otherwise = (Map.insert i (n { timeout = Duration (d - 1) }) acc, rms)
    where (Duration d) = timeout n

handleResult :: RequestNameReply -> Maybe String
handleResult NamePrimaryOwner = Nothing
handleResult NameAlreadyOwner = Nothing
handleResult NameInQueue = Just "waiting in queue"
handleResult NameExists = Just "name is already taken, cannot take over service"
handleResult _ = Just "something whent wrong"


data ClosingReason = Expired | Dismissed | CloseCalled | Undefined deriving (Eq, Show)

closingReasonVariant :: ClosingReason -> Variant
closingReasonVariant Expired = toVariant (1 :: Word32)
closingReasonVariant Dismissed = toVariant (2 :: Word32)
closingReasonVariant CloseCalled = toVariant (3 :: Word32)
closingReasonVariant Undefined = toVariant (4 :: Word32)

closeSignal :: ID -> ClosingReason -> Signal
closeSignal i r = Signal notifyPath (interfaceName_ notifyBus) "NotificationClosed" Nothing Nothing [toVariant i, closingReasonVariant r]

