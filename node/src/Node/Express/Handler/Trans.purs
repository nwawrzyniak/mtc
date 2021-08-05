module Node.Express.Handler.Trans where

import Prelude
import Node.Express.Handler (HandlerM, next, nextThrow) as H
import Effect.Exception (Error)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Logger.Trans (class MonadLogger, LoggerT(..), runLoggerT)
import Control.Monad.Logger.Class (log)
import Node.Express.Response (sendJson, setStatus)
import Logging (warn')

class MonadHandler m where
  liftHandler :: forall a. H.HandlerM a -> m a

instance handlerTrans :: MonadHandler H.HandlerM where
  liftHandler a = a

instance monadTransHandler :: (MonadTrans mt, Monad ma, MonadHandler ma) => MonadHandler (mt ma) where
  liftHandler = lift <<< liftHandler

next :: forall m. MonadHandler m => m Unit
next = liftHandler H.next

nextThrow :: forall m a. MonadHandler m => Error -> m a
nextThrow = liftHandler <<< H.nextThrow
