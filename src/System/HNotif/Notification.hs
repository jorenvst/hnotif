module System.HNotif.Notification where

import System.HNotif.Types
import System.HNotif.Configuration

import DBus

import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Word (Word32)
import Data.Int (Int32)

import Data.Map (Map)

hasExpired :: Notification -> IO Bool
hasExpired n
    | isNothing $ expirationTime n = return False
    | otherwise = (> m) <$> getPOSIXTime
    where (Just m) = expirationTime n

makeNotification :: HNotifConfig -> String -> Word32 -> FilePath -> String -> String -> [ String ] -> Map String Variant -> Int32 -> IO (ID, Notification)
makeNotification config a i _ s b _ _ et = fmap (\t -> (i, Notification a s b t)) (expiration (makeDuration config et))

makeDuration :: HNotifConfig -> Int32 -> Duration
makeDuration _ 0 = Nothing
makeDuration config (-1) = defaultTimeout config
makeDuration _ d = Just d
