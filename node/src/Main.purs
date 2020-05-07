module Main where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.Identity (Identity)
import Data.DateTime (adjust)
import Data.Newtype (wrap)
import Data.DateTime.Instant (fromDateTime, toDateTime)
import Data.Time.Duration (Days(..))
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.AVar (new)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, Fiber, launchAff, launchAff_, bracket)
import Effect.Timer (setInterval)
import Effect.Now (now)
import Node.FS (FileDescriptor, FileFlags(A)) as FS
import Node.FS.Aff (exists, mkdir, fdOpen, fdClose)
import Node.Express.App (AppM, App)
import Node.Express.Ws (listenHostHttpWs)
import Node.Express.Middleware.Static (static)
import Node.HTTP (Server)
import Node.Process (lookupEnv)


import Control.Monad.Logger.Class (log) as L

import Types (ApplicationM, instantToTimestamp)
import Logging (class MonadLogger, Logger, LoggerT(..), LogLevel(Debug, Info), fileLogger, consoleLogger, minimumLevel, runLogger, runLoggerT)
import Database (DBConnection, newDB, closeDB, prepareDb, sqlCreateTableIfNotExists, sqlRemoveOldMessages)
import Handlers ( errorHandler, addMessageHandler, parseBody, wsHandler)
import Node.Express.App.Trans (class MonadApp, liftApp, get, post, ws, useOnError, useAt)

-- | Parse a `String` to an `Int` defaulting to 0 on failiure
parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

-- | Application configuration. Mostly routing
mkApp :: DBConnection -> ApplicationM Effect (AppM Unit)
mkApp db = do
    let static' = static "./static/"
    connClients <- liftEffect $ new []
--    ws    "/ws/test"     $ echo
    pure $ do
      ws    "/chat"        $ wsHandler db connClients
      get   "/"            $ static'
      get   "/style.css"   $ static'
      get   "/main.min.js" $ static'
      useAt "/api/msg"     $ parseBody
      post  "/api/msg"     $ addMessageHandler  db connClients
    --useOnError           test3

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

mkLogger :: forall m. MonadEffect m => FS.FileDescriptor -> Logger m
mkLogger fd = let logger1 = fileLogger fd # minimumLevel Debug
                  logger2 = consoleLogger # minimumLevel Info
              in  logger1 <> logger2

-- | main. Starts a server.
main :: Effect (Fiber Server)
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  launchAff $ bracket (do
                        db <- initDB
                        logFd <- fdOpen "test.log" FS.A Nothing
                        pure {logFd, db}
                      )
                      ( \{logFd, db} -> do
                         fdClose logFd
                         closeDB db
                      )
    \{logFd, db} -> liftEffect do
      _ <- setInterval (60*1000) (removeOldMsg db)
      let logger = mkLogger logFd
      app <- runLogger (mkApp db) logger
      listenHostHttpWs app port "127.0.0.1" \_ ->
        log $ "Listening on " <> show port
