module Database ( module Export, prepareDb, sqlRemoveOldMessages, sqlGetMessages
                , sqlGetNewerMessages , sqlInsertMessage
                , sqlCreateTableIfNotExists) where

import Data.Newtype (unwrap)
import Foreign (Foreign)
import Effect.Aff (Aff)
import SQLite3 (DBConnection, Query, queryObjectDB)
import SQLite3 (DBConnection, newDB, closeDB) as Export
import Types

-- | Represents a sql query with parameters
type ParamedQuery param = { query :: Query, params :: Record param }

-- | Used to execute querys with parameters
type PrepedDb = forall param. ParamedQuery param -> Aff Foreign

-- | Takes a `DBConnection` and transforms it into a `PrepedDb`
prepareDb :: DBConnection -> PrepedDb
prepareDb db { query, params } = queryObjectDB db query params

-- | Builds a query which deletes messages before given `Timestamp`
sqlRemoveOldMessages :: Timestamp -> ParamedQuery ( "$timestamp" :: Int )
sqlRemoveOldMessages t = { query: "DELETE FROM `msg` WHERE `timestamp` < $timestamp;"
                         , params: { "$timestamp": unwrap t }
                         }

-- | Query which selects all messages
sqlGetMessages :: ParamedQuery ()
sqlGetMessages = { query: "SELECT `msg`, `timestamp` FROM `msg`;"
                 , params: {}
                 }

-- | Builds a query which selects all messages after specified `Timestamp`
sqlGetNewerMessages :: Timestamp -> ParamedQuery ( "$timestamp" :: Int )
sqlGetNewerMessages t = { query: "SELECT `msg`, `timestamp` FROM `msg` WHERE `timestamp` >= $timestamp;"
                        , params: { "$timestamp": unwrap t }
                        }

-- | Builds a query to insert a `Msg` into the database
sqlInsertMessage ::  Msg -> ParamedQuery ( "$msg" :: String, "$timestamp" :: Int)
sqlInsertMessage msg = { query: "INSERT INTO `msg` (`msg`, `timestamp`) VALUES ($msg, $timestamp);"
                       , params: { "$msg": msg.msg
                                 , "$timestamp": unwrap msg.timestamp
                                 }
                       }

-- | Query which creates the message table if it is not present
sqlCreateTableIfNotExists :: ParamedQuery ()
sqlCreateTableIfNotExists =
  { query: "CREATE TABLE IF NOT EXISTS `msg`( `msg` text, `timestamp` int);"
  , params: {}
  }
