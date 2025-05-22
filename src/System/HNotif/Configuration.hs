module System.HNotif.Configuration where

import System.HNotif.Types

type Offset = (Int, Int)
type Spacing = Int
type NotificationSize = (Int, Int)

data HNotifConfig = HNotifConfig
    { defaultTimeout :: Duration
    , updateTime :: Int
    , spawn :: Int -> Maybe (Int, Int) -> NotificationSize -> (Int, Int)  -- Maybe (Int, Int) represents the spawnposition of the previous notification (bottom right corner)
    , defaultSize :: NotificationSize
    , spacing :: (Spacing, Spacing)
    }

columnSpawn :: (Spacing, Spacing) -> Int -> Maybe (Int, Int) -> NotificationSize -> (Int, Int)
columnSpawn (sx,sy) _ Nothing _ = (sx,sy)
columnSpawn (sx,sy) _ (Just (_,prevY)) _ = (sx, prevY + sy)

rowSpawn :: (Spacing, Spacing) -> Int -> Maybe (Int, Int) -> NotificationSize -> (Int, Int)
rowSpawn (sx,sy) _ Nothing _ = (sx, sy)
rowSpawn (sx,sy) _ (Just (prevX,_)) _ = (prevX + sx, sy)

defHNotifConfig :: HNotifConfig
defHNotifConfig = HNotifConfig
    { defaultTimeout = seconds 2
    , updateTime = 100000
    , spawn = columnSpawn (spacing defHNotifConfig)
    , defaultSize = (200, 100)
    , spacing = (10, 10)
    }

