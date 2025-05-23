module System.HNotif.Configuration where

import System.HNotif.Types

type Offset = (Int, Int)
type Spacing = Int
type NotificationSize = (Int, Int)

data TextAlignment = LeftAlignment | RightAlignment | CenterAlignment

data NotificationViewConfig = NotificationViewConfig
    { summaryAlignment :: TextAlignment 
    , bodyAlignment :: TextAlignment
    , padding :: Int
    , defaultSize :: NotificationSize   -- minimum size for a notification
    }

data HNotifConfig = HNotifConfig
    { defaultTimeout :: Duration    -- how long a notification should be shown
    , updateTime :: Int     -- how often the service refreshes (microseconds)
    , spawn :: Int -> Maybe (Int, Int) -> NotificationSize -> (Int, Int)  -- Maybe (Int, Int) represents the spawnposition of the previous notification (bottom right corner)
    , spacing :: (Spacing, Spacing)     -- spacing between notifications x and y respectively
    , notificationView :: NotificationViewConfig
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
    , updateTime = 1000
    , spawn = columnSpawn (spacing defHNotifConfig)
    , spacing = (10, 10)
    , notificationView = defNotificationView
    }

defNotificationView :: NotificationViewConfig
defNotificationView = NotificationViewConfig
    { summaryAlignment = LeftAlignment
    , bodyAlignment = LeftAlignment
    , padding = 10
    , defaultSize = (200, 100)
    }

