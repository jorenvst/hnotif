{-# LANGUAGE OverloadedStrings #-}
module System.HNotif.DBus.Notification where

import System.HNotif.Configuration
import System.HNotif.Types
import System.HNotif.Notification

import DBus
import DBus.Client

import Data.Word (Word32)
import Data.Int (Int32)
import Data.Typeable (Typeable)

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar, modifyTVar)
import Control.Exception (Exception, throwIO)

-- adding notifications
handleNotification :: HNotifConfig -> HNotifStateVar -> String -> Word32 -> FilePath -> String -> String -> [ String ] -> Map String Variant -> Int32 -> IO ID
handleNotification config state a b c d e f g h = makeNotification config a b c d e f g h >>= onNotificationIO config state

onNotificationIO :: HNotifConfig -> HNotifStateVar -> (ID, Notification) -> IO ID
onNotificationIO config state (i,n) = atomically $ modifyTVar state (onNotification config (i,n)) >> return i

onNotification :: HNotifConfig -> (ID, Notification) -> HNotifState -> HNotifState
onNotification _ (i,n) state = newState
    where newState = state { notifications = Map.insert i n (notifications state) }

-- closing notifications
data EmptyError = EmptyError deriving (Typeable)
instance Show EmptyError where
    show _ = ""
instance Exception EmptyError

data CloseReason = Expired | Dismissed | CloseCalled | Undefined deriving (Eq, Show, Enum)

reasonToVariant :: CloseReason -> Variant
reasonToVariant = toVariant . (fromIntegral :: Int -> Int32) . (+1) . fromEnum

handleCloseNotification :: Client -> HNotifConfig -> HNotifStateVar -> ID -> IO ()
handleCloseNotification client config state i = do
    ns <- readTVarIO state >>= onCloseNotification config i
    atomically $ writeTVar state ns
    emit client $ closeSignal i CloseCalled

onCloseNotification :: HNotifConfig -> ID -> HNotifState -> IO HNotifState
onCloseNotification _ i state
    | Map.member i (notifications state) = return state { notifications = Map.delete i (notifications state) }
    | otherwise = throwIO EmptyError


closeSignal :: ID -> CloseReason -> Signal
closeSignal i r = (signal "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "NotificationClosed")
    { signalBody = [ toVariant i, reasonToVariant r ] }
