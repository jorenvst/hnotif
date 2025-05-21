module System.HNotif.Notifications where

import DBus
import DBus.Client

import Control.Monad.Trans.State
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Maybe 
import Control.Exception (throwIO, Exception)
import Data.Typeable (Typeable)

-- TODO move to separate module
-- duration in milliseconds
data Timeout = Duration Int32 | DefaultDuration | Forever deriving (Eq, Show)

notificationTimeout :: Int32 -> Timeout
notificationTimeout n
    | n == 0 = Forever
    | n == -1 = DefaultDuration
    | otherwise = Duration n

isDuration :: Timeout -> Bool
isDuration (Duration _) = True
isDuration _ = False

isDefaultDuration :: Timeout -> Bool
isDefaultDuration DefaultDuration = True
isDefaultDuration _ = False

isForever :: Timeout -> Bool
isForever Forever = True
isForever _ = False

seconds :: Int32 -> Timeout
seconds = Duration . (*1000)

type ID = Word32
data Notification = Notification
    { appName :: String
    , replacesId :: ID
    , appIcon :: FilePath
    , summary :: String
    , body :: String
    , actions :: [ String ]
    , hints :: Map String Variant
    , timeout :: Timeout
    } deriving (Eq, Show)

data EmptyError = EmptyError deriving (Typeable)
instance Show EmptyError where
    show _ = ""
instance Exception EmptyError

notify :: Client -> IORef (Map ID Notification) -> String -> Word32 -> FilePath -> String -> String -> [ String ] -> Map String Variant -> Int32 -> IO ID
notify _ ref an rid ai sm b as hs et = do
    let n = Notification an rid ai sm b as hs (notificationTimeout et)
    modifyIORef ref (Map.insert rid n)
    return rid

closeNotification :: Client -> IORef (Map ID Notification) -> ID -> IO ()
closeNotification client ref rid = do
    notifications <- readIORef ref
    handleClose notifications rid
    modifyIORef ref (Map.delete rid)

handleClose :: Map ID Notification -> ID -> IO ()
handleClose notifications rid
    | isNothing $ Map.lookup rid notifications = throwIO EmptyError
    | otherwise = return ()
