module Handlers ( errorHandler, getMessagesHandler, getNewerMessagesHandler
                , addMessageHandler, parseBody
                ) where

import Prelude hiding (apply)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, message, error)
import Effect.Aff.Class (liftAff)
import Node.Express.Request (getRequestHeader, getBody, getMethod)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Handler (Handler, next)
import Node.Express.Types (Method (POST))
import SQLite3 (DBConnection)
import Effect.Now (now)
import Middleware.Middleware as Middleware
import Types (Timestamp(..), RawTimestamp, RawMsg, instantToTimestamp, opSucceded, opFailed)
import Database (prepareDb, sqlGetMessages, sqlInsertMessage
                , sqlGetNewerMessages)

-- | `Handler` to respond with 400 and the produced error
errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

-- | `Handler` which responds with all messages in the database in json format
getMessagesHandler :: DBConnection -> Handler
getMessagesHandler db = do
  let db' = prepareDb db
  msgs <- liftAff $ db' sqlGetMessages
  sendJson msgs

-- | `Handler` which responds with new messages.
-- | This handler tries to extract the `RawTimestamp` from the request body
-- | or fails with `opFailed`. If successful it responds with all messages
-- | after given timestamp
getNewerMessagesHandler :: DBConnection -> Handler
getNewerMessagesHandler db = do
  let db' = prepareDb db
  body <- getBody
  case runExcept body of
    Right ({timestamp: mts} :: RawTimestamp) -> do
      case fromString mts of
        Just ts -> do
          msgs <- liftAff $ db' $ sqlGetNewerMessages $ Timestamp ts
          sendJson msgs
        Nothing-> do
          liftEffect $ log "given timestamp is not an int"
          sendJson opFailed
    Left e -> do
      liftEffect $ log $ show e
      sendJson opFailed

-- | `Handler` which adds a message to the database.
-- | It tries to extract the `RawMsg` from the request body, failing with
-- | `opFailed` if unsuccessful, else it adds the message with the current
-- | timestamp to the database responding `opSuccess`
addMessageHandler :: DBConnection -> Handler
addMessageHandler db = do
    body <- getBody
    case runExcept body of
      Right ({msg: msg} :: RawMsg) ->
          let msg' = case msg of
                ""        -> "\n\r"
                otherwise -> msg
          in do
            ts <- liftEffect $ instantToTimestamp <$> now
            _ <- liftAff $ db' $ sqlInsertMessage {msg: msg', timestamp: ts}
            sendJson opSucceded
      Left e -> do
          liftEffect $ log $ show e
          sendJson opFailed
  where db' = prepareDb db

-- | Middleware (see expressjs) to parse the body of a post request depending on
-- | the requests `Content-Type` header
parseBody :: Handler
parseBody = do
  getMethod >>= case _ of
    POST -> getRequestHeader "Content-Type" >>= case _ of
      Just "application/x-www-form-urlencoded; charset=UTF-8"
            -> Middleware.urlencoded
      Just "application/x-www-form-urlencoded"
            -> Middleware.urlencoded
      Just "application/json; charset=UTF-8"
            -> Middleware.json
      Just "application/json"
            -> Middleware.json
      Just contentType -> throwError $ error $ "Unknown Content-Type: " <> contentType
      Nothing          -> throwError $ error $ "Content-Type not present"
    _ -> next
