module Main where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.DateTime (adjust)
import Data.DateTime.Instant (fromDateTime, toDateTime)
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, Fiber, launchAff, launchAff_)
import Effect.Timer (setInterval)
import Effect.Now (now)
import Node.FS.Aff (exists, mkdir)
import Node.Express.App (App, listenHostHttp, get, post, useOnError, useAt)
import Node.Express.Middleware.Static (static)
import Node.HTTP (Server)
import Node.Process (lookupEnv)
import SQLite3 (DBConnection, newDB)

import Types (instantToTimestamp)
import Database (prepareDb, sqlCreateTableIfNotExists, sqlRemoveOldMessages)
import Handlers ( errorHandler, getMessagesHandler, addMessageHandler
                , getNewerMessagesHandler, parseBody)

-- | Parse a `String` to an `Int` defaulting to 0 on failiure
parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

-- | Application configuration. Mostly routing
app :: DBConnection -> App
app db = do
    let static' = static "./static/"
    get   "/"            $ static'
    get   "/style.css"   $ static'
    get   "/main.min.js" $ static'
    useAt "/api/get"     $ parseBody
    get   "/api/get"     $ getMessagesHandler db
    post  "/api/get"     $ getNewerMessagesHandler db
    useAt "/api/msg"     $ parseBody
    post  "/api/msg"     $ addMessageHandler  db
    useOnError           $ errorHandler

-- | Initializer for the database
initDB :: Aff DBConnection
initDB = do
  let dataFolder = "./data/"
      dbFilepath = dataFolder <> "data.sqlite3"
  (flip unless) (mkdir dataFolder) =<< exists dataFolder
  db <- newDB dbFilepath
  let db' = prepareDb db
  _ <- db' sqlCreateTableIfNotExists
  pure db

-- | Function to remove (7 days) old messages
removeOldMsg :: DBConnection -> Effect Unit
removeOldMsg db = do
  let db' = prepareDb db
  mts <-  shift <$> now
  case instantToTimestamp <$> mts of
    Just ts -> do
        log $ "Clearing old msg's before '" <> show ts <> "'"
        launchAff_ $ db' $ sqlRemoveOldMessages ts
    Nothing ->
        log "Unable to compute Timestamp"
  where shift = map fromDateTime <<< adjust (Days (-7.0)) <<< toDateTime

-- | main. Starts a server.
main :: Effect (Fiber Server)
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  launchAff do
    db <- initDB
    liftEffect do
      _ <- setInterval (60*1000) (removeOldMsg db)
      listenHostHttp (app db) port "127.0.0.1" \_ ->
        log $ "Listening on " <> show port
