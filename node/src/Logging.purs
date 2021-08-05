module Logging
( module Data.Log.Level, module Control.Monad.Logger.Trans, module LExport
, Logger, fileFormatter, fileLogger, consoleLogger, runLogger, minimumLevel
, trace', debug', info', warn', error') where

import Data.JSDate (toISOString)
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Log.Message (Message)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Map (empty)
import Data.String (joinWith)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as C
import Control.Monad.Logger.Trans (class MonadLogger, LoggerT(..), runLoggerT, trace, debug, info, warn, error)
import Control.Logger (cfilter) as LExport
import Control.Logger (Logger(Logger), log, cfilter) as L
import Node.Buffer.Class (fromString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS (FileDescriptor)
import Node.FS.Sync (fdAppend)
import Prelude

type Logger m = L.Logger m Message

fileFormatter :: forall m. MonadEffect m => Message -> m String
fileFormatter { level, timestamp, message } =
  liftEffect $ toISOString timestamp <#> \ts ->
    joinWith " "
      [ showLevel level
      , ts
      , message
      ]

showLevel :: LogLevel -> String
showLevel Trace = "[TRACE]"
showLevel Debug = "[DEBUG]"
showLevel Info  = "[INFO] "
showLevel Warn  = "[WARN] "
showLevel Error = "[ERROR]"

trace' :: forall m. MonadLogger m => String -> m Unit
trace' = trace empty

debug' :: forall m. MonadLogger m => String -> m Unit
debug' = debug empty

info' :: forall m. MonadLogger m => String -> m Unit
info'  = info empty

warn' :: forall m. MonadLogger m => String -> m Unit
warn'  = warn empty

error' :: forall m. MonadLogger m => String -> m Unit
error' = error empty

fileLogger :: forall m. MonadEffect m => FileDescriptor -> Logger m
fileLogger fd = L.Logger $ fileFormatter >=> flip Buffer.fromString UTF8 >>> liftEffect >=> fdAppend fd >>> liftEffect >>> void

consoleLogger :: forall m. MonadEffect m => Logger m
consoleLogger = L.Logger $ prettyFormatter >=> C.log >>> liftEffect

runLogger :: forall m a. LoggerT m a -> Logger m -> m a
runLogger = runLoggerT >>> (L.log >>> _)

minimumLevel :: forall m. (Applicative m) => LogLevel -> Logger m -> Logger m
minimumLevel level = L.cfilter $ (level <= _) <<< _.level
