module System.HNotif.Meta where

-- capabilities
capabilities :: IO [ String ]
capabilities = return
    [ "body"
    ]

-- server info
type Name = String
type Vendor = String
type Version = String
type SpecVersion = String

serverInformation :: IO (Name, Vendor, Version, SpecVersion)
serverInformation = return
    ( "hnotif"
    , "jorenvst"
    , "0.1"
    , "1.3"
    )
