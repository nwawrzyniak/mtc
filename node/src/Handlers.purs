module Handlers ( errorHandler, addMessageHandler, parseBody, wsHandler
                ) where

import Prelude hiding (apply)
import Data.Array (cons)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExcept)
import Control.Monad.Loops (untilM_)
import Control.Parallel (parTraverse)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, message, error)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.AVar (AVar, take, put, empty, read)
import Foreign.Generic (encodeJSON)
import Node.Express.Request (getRequestHeader, getBody, getMethod)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Handler (Handler, next)
import Node.Express.Types (Method (POST))
import Node.Express.Ws (WebSocket, WsReqHandler, getSocket, send)
import Simple.JSON (read) as JSON
import SQLite3 (DBConnection)
import Effect.Now (now)
import Middleware.Middleware as Middleware
import Types (RawMsg, Msg, instantToTimestamp, msgToRaw, opSucceded, opFailed)
import Database (prepareDb, sqlGetMessages, sqlInsertMessage)

-- | `Handler` to respond with 400 and the produced error
errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

{-}
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
-}
-- | `Handler` which adds a message to the database.
-- | It tries to extract the `RawMsg` from the request body, failing with
-- | `opFailed` if unsuccessful, else it adds the message with the current
-- | timestamp to the database responding `opSuccess`
addMessageHandler :: DBConnection -> AVar ConnectedClients -> Handler
addMessageHandler db connClients = do
    body <- getBody
    case runExcept body of
      Right (msgs :: Array RawMsg) -> do
        _ <- (flip traverse) msgs \msg ->
          let msg' = case msg of
                {msg: ""} -> "\n\r"
                {msg: m}  -> m
          in do
              liftEffect $ log "start"
              ts <- liftEffect $ instantToTimestamp <$> now
              let theMsg = {msg: msg', timestamp: ts}
              _ <- liftAff $ do
                    _ <- db' $ sqlInsertMessage theMsg
                    clients <- read connClients
                    liftEffect $ log "start transmit"
                    _ <- parTraverse (\c -> put (msgToRaw theMsg) c.msgCond) clients
                    liftEffect $ log "done transmit"
              pure unit
        sendJson opSucceded
      Left e -> do
          liftEffect $ log $ show e
          sendJson opFailed
  where db' = prepareDb db

type ConnectedClients = (Array {soc :: WebSocket, msgCond :: AVar RawMsg})

withClients :: AVar ConnectedClients
            -> (ConnectedClients -> Aff (ConnectedClients))
            -> Aff Unit
withClients connClients cb = do
  socc <- take connClients
  mSocc' <- try $ cb socc
  case mSocc' of
    (Right socc') -> put socc' connClients
    (Left e) -> do
      put socc connClients
      throwError e

wsHandler :: DBConnection -> AVar ConnectedClients -> WsReqHandler
wsHandler db connClients = do
  let db' = prepareDb db
  liftEffect $ log $ "[ws] client connected"
  clMsgCond <- liftAff $ empty
  socket <- getSocket
  liftAff $ withClients connClients $ pure <<< cons {soc: socket, msgCond: clMsgCond}
  liftEffect $ log $ "[ws] client added"
  results <- liftAff $ JSON.read <$> db' sqlGetMessages
  send <<< encodeJSON $ case results of
    Right (as :: Array Msg) -> msgToRaw <$> as
    Left e -> [{msg: "Error! Row didn't deserialize correctly :( "}]
  untilM_ ( do
             msg <- liftAff $ take clMsgCond
             send $ encodeJSON [msg]
          ) $ pure false
  pure unit





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
