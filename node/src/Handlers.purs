module Handlers where

import Prelude hiding (apply)
import Data.Int (round)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (wrap, unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (Error, message)
import Effect.Aff.Class (liftAff)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Handler (Handler)
import SQLite3 (DBConnection)
import Effect.Now (now)


import Database (prepareDb, sqlGetMessages, sqlInsertMessage)

errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

getMessagesHandler :: DBConnection -> Handler
getMessagesHandler db = do
  let db' = prepareDb db
  sendJson $ db' sqlGetMessages

addMessageHandler :: DBConnection -> Handler
addMessageHandler db = do
    --TODO: get message
    let msg = ""
    ts <- liftEffect $ convert <$> now
    _ <- liftAff $ db' $ sqlInsertMessage {msg: msg, timestamp: ts}
    sendJson {success: "true"}
  where db' = prepareDb db
        convert = wrap <<< round <<< unwrap <<< unInstant
