module Main where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
--import Data.Identity (Identity)
import Data.DateTime (adjust)
--import Data.Newtype (wrap, unwrap)
import Data.DateTime.Instant (fromDateTime, toDateTime)
import Data.Time.Duration (Days(..))
--import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.AVar (new)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Aff (Aff, launchAff_, bracket)
import Effect.Timer (setInterval)
import Effect.Now (now)
import Node.FS (FileDescriptor, FileFlags(A)) as FS
import Node.FS.Aff (exists, mkdir, fdOpen, fdClose)
import Node.Express.App (AppM)
import Node.Express.Ws (listenHostHttpWs)
import Node.Express.Middleware.Static (static)
import Node.HTTP (Server, close)
import Node.Process (lookupEnv)
import Node.ReadLine.Aff (Interface, createConsoleInterface, prompt, noCompletion)


--import Control.Monad.Logger.Class (log) as L

import Types (ApplicationM, instantToTimestamp)
import Logging (Logger, LogLevel(Debug, Info), fileLogger, consoleLogger,
                minimumLevel, runLogger, info')
import Database (DBConnection, newDB, closeDB, prepareDb, sqlCreateTableIfNotExists, sqlRemoveOldMessages)
import Handlers ( addMessageHandler, parseBody, wsHandler)
import Node.Express.App.Trans (get, post, ws, useAt)

-- | Parse a `String` to an `Int` defaulting to 0 on failiure
parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

-- | Application configuration. Mostly routing
mkApp :: DBConnection -> ApplicationM Aff (AppM Unit)
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
removeOldMsg :: DBConnection -> ApplicationM Aff Unit
removeOldMsg db = do
  let db' = prepareDb db
  mts <-  liftEffect $ shift <$> now
  case instantToTimestamp <$> mts of
    Just ts -> do
        info' $ "Clearing old msg's before '" <> show ts <> "'"
        liftEffect $ launchAff_ $ db' $ sqlRemoveOldMessages ts
    Nothing ->
        info' "Unable to compute Timestamp"
  where shift = map fromDateTime <<< adjust (Days (-7.0)) <<< toDateTime

mkLogger :: forall m. MonadEffect m => FS.FileDescriptor -> Logger m
mkLogger fd = let logger1 = fileLogger fd # minimumLevel Debug
                  logger2 = consoleLogger # minimumLevel Info
              in  logger1 <> logger2

commandLoop :: Interface -> Server -> ApplicationM Aff Unit
commandLoop interface server = do
  command <- prompt interface
  stop <- case command of
    "" -> pure false
    "help" -> do
      log "Available commands: stop, help"
      pure false
    "stop" -> pure true
    _ -> do
      log "Unknown command! Try help!"
      pure false
  case stop of
    true -> do
      info' "Stopping server"
      liftEffect $ close server (log "Server stopped")
    false -> commandLoop interface server

-- | main. Starts a server.
main :: Effect Unit--(Fiber Server)
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  interface <- createConsoleInterface noCompletion
  launchAff_ $ bracket
    (do --init resources
      log "Open DB connection"
      db <- initDB
      log "Open log file"
      logFd <- fdOpen "test.log" FS.A Nothing
      pure {logFd, db}
    )
    ( \{logFd, db} -> do  --cleanup resources
       log "Closing DB connection"
       closeDB db
       log "Closing log file"
       fdClose logFd
    )
    \{logFd, db} -> do
      let logger :: Logger Aff
          logger = mkLogger logFd
      app <- runLogger (mkApp db) logger
      server <- liftEffect do
        _ <- setInterval (60*1000) $ launchAff_ $ runLogger (removeOldMsg db) logger
        listenHostHttpWs app port "127.0.0.1" \_ ->
          launchAff_ $ runLogger (info' $ "Server listening on " <> show port) logger
      runLogger
        ( do
          removeOldMsg db
          info' "Start listening for commands"
          commandLoop interface server
          info' "Exiting application"
        ) logger
