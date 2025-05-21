module System.HNotif.Configuration where

import System.HNotif.Types

data HNotifConfig = HNotifConfig
    { defaultTimeout :: Duration
    , updateTime :: Int
    }

defHNotifConfig :: HNotifConfig
defHNotifConfig = HNotifConfig
    { defaultTimeout = seconds 2
    , updateTime = 100000
    }

