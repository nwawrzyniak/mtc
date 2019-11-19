module Handlers where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Unit (unit)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Effect.Class (liftEffect)
import Effect.Exception (Error, message, error)
import Effect.Aff.Class (liftAff)
import Node.Express.Request (getRequestHeader, getBody, getBody')
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Handler (Handler)
import SQLite3 (DBConnection)
import Effect.Now (now)
import Foreign (F, tagOf, typeOf)
import Middleware.Middleware as Middleware


import Effect.Console (log)


import Types (RawMsg, instantToTimestamp)
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
    unit <- parseBody
    Middleware.debugLog
    (body :: F RawMsg) <- getBody
    b' <- getBody'
    liftEffect do
        log $ show $ runExcept body
        log $ tagOf b'
        log $ typeOf b'
    case runExcept body of
      Right ({"msg": msg} :: RawMsg) -> do
          ts <- liftEffect $ instantToTimestamp <$> now
          _ <- liftAff $ db' $ sqlInsertMessage {msg: msg, timestamp: ts}
          sendJson {success: "true"}
      Left e -> throwError $ error $ show e
  where db' = prepareDb db

parseBody :: Handler
parseBody = getRequestHeader "Content-Type" >>= case _ of
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
