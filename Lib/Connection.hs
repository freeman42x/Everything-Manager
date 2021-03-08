module Lib.Connection where

import Database.PostgreSQL.Simple (Connection, connect, ConnectInfo(..))

getDbConn :: IO Connection
getDbConn = connect ConnectInfo
  { connectHost = "localhost"
  , connectPort = 5432
  , connectDatabase = "everythingmanager"
  , connectUser = "postgres"
  , connectPassword = "pass1234"
  }
