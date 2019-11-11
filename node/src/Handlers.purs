module Handlers where

import Prelude hiding (apply)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.DateTime.Instant (unInstant)
import Data.Newtype (wrap, unwrap)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Effect.Class (liftEffect)
import Effect.Exception (Error, message, error)
import Effect.Aff.Class (liftAff)
import Node.Express.Request (getRequestHeader, getBody)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Handler (Handler)
import SQLite3 (DBConnection)
import Effect.Now (now)
import Middleware.Middleware as Middleware


import Database (prepareDb, sqlGetMessages, sqlInsertMessage)
import Types (RawMsg)

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
    parseBody
    body <- getBody
    case runExcept body of
      Right ({"msg": msg} :: RawMsg) -> do
          ts <- liftEffect $ convert <$> now
          _ <- liftAff $ db' $ sqlInsertMessage {msg: msg, timestamp: ts}
          sendJson {success: "true"}
      Left e -> throwError $ error $ show e
  where db' = prepareDb db
        convert = wrap <<< round <<< unwrap <<< unInstant

parseBody :: Handler
parseBody = getRequestHeader "Content-Type" >>= case _ of
    Just "application/x-www-form-urlencoded" -> Middleware.urlencoded
    Just "application/json"                  -> Middleware.json
    Just contentType -> throwError $ error $ "Unknown Content-Type: " <> contentType
    Nothing          -> throwError $ error $ "Content-Type not present"