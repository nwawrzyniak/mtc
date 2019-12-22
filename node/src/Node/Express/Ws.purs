module Node.Express.Ws where

--import Node.Express.Types (Method(..))

import Prelude hiding (apply)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Foreign (Foreign, unsafeToForeign)
import Node.Express.App (App, AppM(..), apply)
import Node.Express.Types (Application, Event, Request, class RoutePattern, Port, Host)
import Node.HTTP (Server)
import Simple.JSON (class ReadForeign, E, read)

foreign import data WebSocket :: Type

class WsHandler m r | m -> r where
  send :: String -> m Unit
  run :: m Unit -> WebSocket -> r -> Effect Unit
  getSocket :: m WebSocket

newtype WsReqHandlerM a = WsReqHandlerM (WebSocket -> Request -> Aff a)

type WsReqHandler = WsReqHandlerM Unit

--derive instance newtypeReqHandler :: Newtype (WsReqHandlerM a) _

instance functorReqHandlerM :: Functor WsReqHandlerM where
    map f (WsReqHandlerM h) = WsReqHandlerM \socket req ->
        (h socket req >>= \r -> pure $ f r)

instance applyReqHandlerM :: Apply WsReqHandlerM where
    apply (WsReqHandlerM f) (WsReqHandlerM h) = WsReqHandlerM \socket req -> do
        trans <- f socket req
        res   <- h socket req
        pure $ trans res

instance applicativeReqHandlerM :: Applicative WsReqHandlerM where
    pure x = WsReqHandlerM \_ _ -> pure x

instance bindReqHandlerM :: Bind WsReqHandlerM where
    bind (WsReqHandlerM h) f = WsReqHandlerM \socket req -> do
        (WsReqHandlerM g) <- liftM1 f $ h socket req
        g socket req

instance monadReqHandlerM :: Monad WsReqHandlerM

instance monadEffReqHandlerM :: MonadEffect WsReqHandlerM where
    liftEffect act = WsReqHandlerM \_ _ -> liftEffect act

instance monadAffReqHandlerM :: MonadAff WsReqHandlerM where
    liftAff act = WsReqHandlerM \_ _ -> act

instance reqHandler :: WsHandler WsReqHandlerM Request where
  send msg = WsReqHandlerM \socket _ -> liftEffect $ _send socket msg
  run (WsReqHandlerM h) socket req = void $ launchAff_ $ h socket req
  getSocket = WsReqHandlerM \socket _ -> pure socket

instance monadThrowReqHandlerM :: MonadThrow Error WsReqHandlerM where
  throwError e = WsReqHandlerM \_ _ -> throwError e

instance monadErrorReqHandlerM :: MonadError Error WsReqHandlerM where
    --forall a. WsReqHandlerM a -> (Error -> WsReqHandlerM a) -> WsReqHandlerM a
    catchError (WsReqHandlerM act) h =
      WsReqHandlerM \socket req -> catchError (act socket req)
        \e -> let (WsReqHandlerM h') = h e
              in h' socket req

newtype WsMsgHandlerM a = WsMsgHandlerM (WebSocket -> Foreign -> Aff a)

type WsMsgHandler = WsMsgHandlerM Unit

--derive instance newtypeMsgHandler :: Newtype (WsMsgHandlerM a) _

instance functorMsgHandlerM :: Functor WsMsgHandlerM where
    map f (WsMsgHandlerM h) = WsMsgHandlerM \socket req ->
        (h socket req >>= \r -> pure $ f r)

instance applyMsgHandlerM :: Apply WsMsgHandlerM where
    apply (WsMsgHandlerM f) (WsMsgHandlerM h) = WsMsgHandlerM \socket req -> do
        trans <- f socket req
        res   <- h socket req
        pure $ trans res

instance applicativeMsgHandlerM :: Applicative WsMsgHandlerM where
    pure x = WsMsgHandlerM \_ _ -> pure x

instance bindMsgHandlerM :: Bind WsMsgHandlerM where
    bind (WsMsgHandlerM h) f = WsMsgHandlerM \socket req -> do
        (WsMsgHandlerM g) <- liftM1 f $ h socket req
        g socket req

instance monadMsgHandlerM :: Monad WsMsgHandlerM

instance monadEffMsgHandlerM :: MonadEffect WsMsgHandlerM where
    liftEffect act = WsMsgHandlerM \_ _ -> liftEffect act

instance monadAffMsgHandlerM :: MonadAff WsMsgHandlerM where
    liftAff act = WsMsgHandlerM \_ _ -> act

instance msgHandler :: WsHandler WsMsgHandlerM Foreign where
  send msg = WsMsgHandlerM \socket _ -> liftEffect $ _send socket msg
  run (WsMsgHandlerM h) socket f = void $ launchAff_ $ h socket f
  getSocket = WsMsgHandlerM \socket _ -> pure socket


foreign import _listenHostHttpWs :: (Application -> Effect Unit)
                                 -> Int
                                 -> String
                                 -> (Event -> Effect Unit)
                                 -> Effect Server
foreign import _ws :: Fn3 Application Foreign (WebSocket -> Request -> Effect Unit) (Effect Unit)
foreign import _onMessage :: WebSocket
                          -> (WebSocket -> Foreign -> Effect Unit)
                          -> Effect Unit
foreign import _onClose :: WebSocket
                        -> Effect Unit
                        -> Effect Unit
foreign import _send :: WebSocket -> String -> Effect Unit

-- | Run application on specified port & host and execute callback after launch.
-- | HTTP & Ws version
listenHostHttpWs :: App -> Port -> Host -> (Event -> Effect Unit) -> Effect Server
listenHostHttpWs app = _listenHostHttpWs $ apply app

onMessage :: WsMsgHandler -> WsReqHandler
onMessage h = WsReqHandlerM \socket _ ->
  liftEffect $ _onMessage socket $ run h

onClose :: Aff Unit -> WsReqHandler
onClose h = WsReqHandlerM \socket _ ->
  liftEffect $ _onClose socket $ launchAff_ h

ws :: forall r. RoutePattern r => r -> WsReqHandler -> App
ws r h = AppM \app -> runFn3 _ws app (unsafeToForeign r) $ run h

getMessage :: forall a. ReadForeign a => WsMsgHandlerM (E a)
getMessage = WsMsgHandlerM $ \_ msg -> pure $ read msg


echo :: WsReqHandler
echo = do
  liftEffect $ log "connected"
  onMessage $ do
    msg <- getMessage >>= pure <<< case _ of
                      Right ms -> ms
                      Left err -> show err
    send msg











--wsMethod :: Method
--wsMethod = CustomMethod "ws"

--newtype WsHandlerM a = WsHandler (WebSocket -> Request -> Aff a)

--type WsHandler = WsHandlerM Unit

--onMessage :: (Foreign -> WsHandler) -> WsHandler
--onMessage act = WsHandler \ws req ->
--    liftEffect $ _onMessage

--onMessage :: Foreign -> WsHandler
--onMessage

--type MessageListener    = Foreign -> Aff Unit
--type ConnectionListener = Aff Unit

--ws :: RoutePattern r => r
--   -> Maybe ConnectionListener
--   -> Maybe MessageListener
--   -> App
--ws r mCl mMl = let cl = fromMaybe _null mCl
--                   ml = fromMaybe _null mMl
--               in AppM \app -> _ws app r cl ml
