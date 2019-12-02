module Node.Express.Ws where

--import Node.Express.Types (Method(..))

import Prelude hiding (apply)
import Data.Function.Uncurried (Fn3)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Foreign (Foreign)
--import Node.Express.App (App(..))
import Node.Express.Types (Application, Event, Request, class RoutePattern)
import Node.HTTP (Server)

foreign import mkApplication :: Effect Application
foreign import _listenHttpWs :: Application -> Int -> (Event -> Effect Unit) -> Effect Server

-- | Run application on specified port & host and execute callback after launch.
-- | HTTP & Ws version
--listenHostHttpWs :: App -> Port -> Host -> (Event -> Effect Unit) -> Effect Server
--listenHostHttpWs (AppM act) port host cb = do
--    app <- mkApplication
--    act app
--    _listenHostHttpWs app port host cb

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






foreign import _ws :: forall r. RoutePattern r => Fn3 Application r WsReqHandler (Effect Unit)
foreign import data WebSocket :: Type

foreign import _onMessage :: WebSocket -> (WebSocket -> Foreign -> Effect Unit) -> Effect Unit
foreign import _send :: WebSocket -> String -> Effect Unit

newtype WsReqHandlerM a = WsReqHandlerM (WebSocket -> Request -> Aff a)
type WsReqHandler = WsReqHandlerM Unit

newtype WsMsgHandlerM a = WsMsgHandlerM (WebSocket -> Foreign -> Aff a)
type WsMsgHandler = WsMsgHandlerM Unit

class WsHandler m where
  send :: String -> m Unit

instance reqHandler :: WsHandler WsReqHandlerM where
  send msg = WsReqHandlerM \ws _ -> liftEffect $ _send ws msg

instance msgHandler :: WsHandler WsMsgHandlerM where
  send msg = WsMsgHandlerM \ws _ -> liftEffect $ _send ws msg

runWsMsg :: WsMsgHandler -> WebSocket -> Foreign -> Effect Unit
runWsMsg (WsMsgHandlerM h) ws f = void $ launchAff_ $ h ws f

runWsReq :: WsReqHandler -> WebSocket -> Request -> Effect Unit
runWsReq (WsReqHandlerM h) ws req = void $ launchAff_ $ h ws req

onMessage :: WsMsgHandler -> WsReqHandler
onMessage h = WsReqHandlerM \ws _ ->
  liftEffect $ _onMessage ws $ runWsMsg h

--ws :: forall r. RoutePattern r => r -> WsReqHandler -> App
--ws r h = AppM \app -> runFn3 _ws app r $ runWsReq h
