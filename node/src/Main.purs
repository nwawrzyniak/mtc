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
import Effect.Aff (Aff, launchAff_)
import Effect.Timer (setInterval)
import Effect.Now (now)
import Node.FS.Aff (exists, mkdir)
import Node.Express.App (App, listenHttp, get, useOnError)
import Node.Express.Response (send)
import Node.Express.Middleware.Static (static)
import Node.HTTP (Server)
import Node.Process (lookupEnv)
import SQLite3 (DBConnection, newDB)

import Types (instantToTimestamp)
import Database (prepareDb, sqlCreateTableIfNotExists, sqlRemoveOldMessages)
import Handlers (errorHandler)


parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


app :: App
app = do
    let static' = static "./static/"
    get "/"          $ static'
    get "/style.css" $ static'
    get "/hello"     $ send "Hello, World!"
    useOnError       $ errorHandler


initDB :: Aff DBConnection
initDB = do
  let dataFolder = "./data/"
      dbFilepath = dataFolder <> "test.sqlite3"
  (flip unless) (mkdir dataFolder) =<< exists dataFolder
  db <- newDB dbFilepath
  let db' = prepareDb db
  _ <- db' sqlCreateTableIfNotExists
  pure db

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

main :: Effect Server
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  _ <- launchAff_ do
    db <- initDB
    liftEffect $ setInterval (60*1000) (removeOldMsg db)
  listenHttp app port \_ ->
      log $ "Listening on " <> show port
