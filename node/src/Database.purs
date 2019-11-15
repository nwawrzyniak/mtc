module Database (prepareDb, sqlRemoveOldMessages, sqlGetMessages, sqlGetNewerMessages
                , sqlInsertMessage, sqlCreateTableIfNotExists) where

import Data.Newtype (unwrap)
import Foreign (Foreign)
import Effect.Aff (Aff)
import SQLite3 (DBConnection, Query, queryObjectDB)

import Types

type ParamedQuery param = { query :: Query, params :: Record param }
type PrepedDb = forall param. ParamedQuery param -> Aff Foreign

prepareDb :: DBConnection -> PrepedDb
prepareDb db { query, params } = queryObjectDB db query params

sqlRemoveOldMessages :: Timestamp -> ParamedQuery ( "$timestamp" :: Int )
sqlRemoveOldMessages t = { query: "DELETE FROM `msg` WHERE `timestamp` < $timestamp;"
                         , params: { "$timestamp": unwrap t }
                         }

sqlGetMessages :: ParamedQuery ()
sqlGetMessages = { query: "SELECT `msg`, `timestamp` FROM `msg`;"
                 , params: {}
                 }

sqlGetNewerMessages :: Timestamp -> ParamedQuery ( "$timestamp" :: Int )
sqlGetNewerMessages t = { query: "SELECT `msg`, `timestamp` FROM `msg` WHERE `timestamp` >= $timestamp;"
                        , params: { "$timestamp": unwrap t }
                        }


sqlInsertMessage ::  Msg -> ParamedQuery ( "$msg" :: String, "$timestamp" :: Int)
sqlInsertMessage msg = { query: "INSERT INTO `msg` (`msg`, `timestamp`) VALUES ($msg, $timestamp);"
                       , params: { "$msg": msg.msg
                                 , "$timestamp": unwrap msg.timestamp
                                 }
                       }

sqlCreateTableIfNotExists :: ParamedQuery ()
sqlCreateTableIfNotExists =
  { query: "CREATE TABLE IF NOT EXISTS `msg`( `msg` text, `timestamp` int);"
  , params: {}
  }
