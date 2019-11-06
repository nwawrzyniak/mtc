module Database where

import Prelude hiding (apply)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Milliseconds(..))
import Foreign (Foreign, unsafeToForeign)
import Effect.Aff (Aff)
import SQLite3 (DBConnection, Query, queryObjectDB)
import Simple.JSON (writeImpl)

import Types



timestamptToStr :: Timestamp -> String
timestamptToStr = show <<< unTimestamp

sqlRemoveOldMessages :: Timestamp -> Query
sqlRemoveOldMessages ts = "DELETE FROM `msg` WHERE `timestamp` < '" 
                       <> timestamptToStr ts
                       <> "';"

sqlGetMessages :: Query
sqlGetMessages = "SELECT `msg`, `timestamp` FROM `msg`;"

sqlCreateTableIfNotExists :: Query
sqlCreateTableIfNotExists =
    """
CREATE TABLE IF NOT EXISTS `msg`
  ( `msg` text
  , `timestamp` int
  );
    """

sqlInsertMessage :: Query
sqlInsertMessage = "INSERT INTO `msg` (`msg`, `timestamp`) VALUES ($msg, $timestamp);"

insertMessage :: DBConnection -> Msg -> Aff Foreign
insertMessage db msg = queryObjectDB db sqlInsertMessage 
                           { "$msg": msg.msg
                           , "$timestamp": unTimestamp msg.timestamp
                           }
