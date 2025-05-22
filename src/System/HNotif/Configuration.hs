module System.HNotif.Configuration where

import System.HNotif.Types

type Offset = (Int, Int)
type Spacing = Int
type NotificationSize = (Int, Int)

data HNotifConfig = HNotifConfig
    { defaultTimeout :: Duration
    , updateTime :: Int
    , spawn :: NotificationSize -> Int -> (Int, Int)
    }

columnSpawn :: Offset -> Spacing -> NotificationSize -> Int -> (Int, Int)
columnSpawn (ox,oy) spacing (_,sy) i = (ox, (sy + spacing) * i + oy)

defHNotifConfig :: HNotifConfig
defHNotifConfig = HNotifConfig
    { defaultTimeout = seconds 2
    , updateTime = 100000
    , spawn = columnSpawn (10, 10) 10
    }

