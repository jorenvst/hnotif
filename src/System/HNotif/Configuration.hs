module System.HNotif.Configuration where

import System.HNotif.Types

type Offset = (Int, Int)
type Spacing = Int
type NotificationSize = (Int, Int)

data HNotifConfig = HNotifConfig
    { defaultTimeout :: Duration
    , updateTime :: Int
    , spawn :: Int -> Maybe (Int, Int) -> NotificationSize -> (Int, Int)  -- Maybe (Int, Int) represents the spawnposition of the previous notification (bottom right corner)
    }

columnSpawn :: Spacing -> Int -> Maybe (Int, Int) -> NotificationSize -> (Int, Int)
columnSpawn spacing _ Nothing _ = (spacing, spacing)
columnSpawn spacing _ (Just (_,prevY)) _ = (spacing, prevY + spacing)

defHNotifConfig :: HNotifConfig
defHNotifConfig = HNotifConfig
    { defaultTimeout = seconds 2
    , updateTime = 100000
    , spawn = columnSpawn 10
    }

