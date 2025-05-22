module System.HNotif.Types where

import Data.Int (Int32)
import Data.Word (Word32)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time.Clock.POSIX

import GHC.Conc (TVar)
import Graphics.UI.Gtk (Window)

-- expiration
type Duration = Maybe Int32
type Expiration = Maybe POSIXTime

seconds :: Int32 -> Duration
seconds n = Just $ n * 1000

expiration :: Duration -> IO Expiration
expiration Nothing = return Nothing
expiration (Just ms) = Just . (+(fromIntegral ms / 1000)) <$> getPOSIXTime

-- notification
type ID = Word32
data Notification = Notification
    { appName :: String
    , summary :: String
    , body :: String
    , expirationTime :: Expiration
    } deriving (Show, Eq)

-- the global state
data HNotifState = HNotifState
    { notifications :: Map ID Notification
    , windows :: Map ID Window
    }
type HNotifStateVar = TVar HNotifState

initialState :: HNotifState
initialState = HNotifState
    { notifications = Map.empty
    , windows = Map.empty
    }
