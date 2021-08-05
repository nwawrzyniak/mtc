module Node.Express.App.Trans
( class MonadApp, liftApp, use, useExternal, useAt, useAtExternal, useOnParam
, useOnError, getProp, setProp, http, get, post, put, delete, ws, all
) where

import Prelude
import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (Error)
import Node.Express.App
  ( AppM, use, useExternal, useAt, useAtExternal, useOnParam, useOnError
  , getProp, setProp, http, get, post, put, delete, all) as App
import Node.Express.Ws (WsReqHandler, ws) as App
import Node.Express.Types (class RoutePattern, Response, Request, Path, Method)
import Node.Express.Handler (Handler)
import Control.Monad.Trans.Class (class MonadTrans, lift)

newtype AppT m a = AppT (App.AppM a -> m a)

--instance functorAppT :: Functor m => Functor (AppT m) where
--  map = map >>> \f (AppT m) -> AppT $ f <<< m

class MonadApp m where
  liftApp :: forall a. App.AppM a -> m a

instance appTrans :: MonadApp App.AppM where
  liftApp a = a

--instance monadTransApp :: (MonadTrans mt, Monad ma, MonadApp ma) => MonadApp (mt ma) where
--  liftApp = lift <<< liftApp

use :: forall m. MonadApp m => Handler -> m Unit
use = liftApp <<< App.use

useExternal :: forall m. MonadApp m => Fn3 Request Response (Effect Unit) (Effect Unit) -> m Unit
useExternal = liftApp <<< App.useExternal

useAt :: forall m. MonadApp m => Path -> Handler ->  m Unit
useAt = (liftApp <<< _) <<< App.useAt

useAtExternal :: forall m. MonadApp m => Path -> Fn3 Request Response (Effect Unit) (Effect Unit) -> m Unit
useAtExternal = (liftApp <<< _) <<< App.useAtExternal

useOnParam :: forall m. MonadApp m => String -> (String -> Handler) ->  m Unit
useOnParam = (liftApp <<< _) <<< App.useOnParam

useOnError :: forall m. MonadApp m => (Error -> Handler) ->  m Unit
useOnError = liftApp <<< App.useOnError

getProp :: forall m a. MonadApp m => String -> m (Maybe a)
getProp = liftApp <<< App.getProp

setProp :: forall m a. MonadApp m => String -> a ->  m Unit
setProp = (liftApp <<< _) <<< App.setProp

http :: forall m r. MonadApp m => (RoutePattern r) => Method -> r -> Handler ->  m Unit
http = ((liftApp <<< _) <<< _) <<< App.http

get :: forall m r. MonadApp m => (RoutePattern r) => r -> Handler ->  m Unit
get = (liftApp <<< _) <<< App.get

post :: forall m r. MonadApp m => (RoutePattern r) => r -> Handler ->  m Unit
post = (liftApp <<< _) <<< App.post

put :: forall m r. MonadApp m => (RoutePattern r) => r -> Handler ->  m Unit
put = (liftApp <<< _) <<< App.put

delete :: forall m r. MonadApp m => (RoutePattern r) => r -> Handler ->  m Unit
delete = (liftApp <<< _) <<< App.delete

ws :: forall m r. MonadApp m => (RoutePattern r) => r -> App.WsReqHandler -> m Unit
ws = (liftApp <<< _) <<< App.ws

all :: forall m r. MonadApp m => (RoutePattern r) => r -> Handler ->  m Unit
all = (liftApp <<< _) <<< App.all
