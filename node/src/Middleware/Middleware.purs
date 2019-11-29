module Middleware.Middleware (json, urlencoded) where

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Unit (Unit)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Request, Response)
import Prelude (($))

foreign import _json       :: Fn3 Request Response (Effect Unit) (Effect Unit)
foreign import _urlencoded :: Fn3 Request Response (Effect Unit) (Effect Unit)

-- | Handler that uses builtin 'json' middleware to parse req.body
json :: Handler
json = HandlerM $
  \req res nxt -> liftEffect $ runFn3 _json req res nxt

-- | Handler that uses builtin 'urlencoded' middleware to parse req.body
urlencoded :: Handler
urlencoded = HandlerM $
  \req res nxt -> liftEffect $ runFn3 _urlencoded req res nxt
