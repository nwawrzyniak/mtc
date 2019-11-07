module Database where

import Prelude hiding (apply)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Milliseconds(..))
import Foreign (Foreign, unsafeToForeign)
import Effect.Aff (Aff)
import SQLite3 (DBConnection, Query, queryObjectDB)
import Simple.JSON (writeImpl)

import Types

type ParamedQuery param = { query :: Query, params :: Record param }
type PrepedDb = forall param. ParamedQuery param -> Aff Foreign

prepareDb :: DBConnection -> PrepedDb
prepareDb db { query, params } = queryObjectDB db query params

sqlRemoveOldMessages :: Timestamp -> ParamedQuery ( "$timestamp" :: Int )
sqlRemoveOldMessages t = { query: "DELETE FROM `msg` WHERE `timestamp` < $timestamp;"
                         , params: { "$timestamp": unTimestamp t }
                         }

sqlGetMessages :: ParamedQuery ()
sqlGetMessages = { query: "SELECT `msg`, `timestamp` FROM `msg`;"
                 , params: {}
                 }

sqlGetNewerMessages :: Timestamp -> ParamedQuery ( "$timestamp" :: Int )
sqlGetNewerMessages t = { query: "SELECT `msg`, `timestamp` FROM `msg` WHERE `timestamp` >= $timestamp;"
                        , params: { "$timestamp": unTimestamp t }
                        }


sqlInsertMessage ::  Msg -> ParamedQuery ( "$msg" :: String, "$timestamp" :: Int)
sqlInsertMessage msg = { query: "INSERT INTO `msg` (`msg`, `timestamp`) VALUES ($msg, $timestamp);"
                       , params: { "$msg": msg.msg
                                 , "$timestamp": unTimestamp msg.timestamp
                                 }
                       }

sqlCreateTableIfNotExists :: Query
sqlCreateTableIfNotExists =
    """
CREATE TABLE IF NOT EXISTS `msg`
  ( `msg` text
  , `timestamp` int
  );
    """
