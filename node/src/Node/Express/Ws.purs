module Node.Express.Ws where

--import Node.Express.Types (Method(..))

import Prelude hiding (apply)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign, unsafeToForeign)
import Node.Express.App (App, AppM(..))
import Node.Express.Types (Application, Event, Request, class RoutePattern, Port, Host)
import Node.HTTP (Server)
import Simple.JSON (read)

foreign import _listenHostHttpWs :: (Application -> Effect Unit)
                                 -> Int
                                 -> String
                                 -> (Event -> Effect Unit)
                                 -> Effect Server

-- | Run application on specified port & host and execute callback after launch.
-- | HTTP & Ws version
listenHostHttpWs :: App -> Port -> Host -> (Event -> Effect Unit) -> Effect Server
listenHostHttpWs (AppM act) = _listenHostHttpWs act



foreign import _ws :: Fn3 Application Foreign (WebSocket -> Request -> Effect Unit) (Effect Unit)
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
  send msg = WsReqHandlerM \socket _ -> liftEffect $ _send socket msg

instance msgHandler :: WsHandler WsMsgHandlerM where
  send msg = WsMsgHandlerM \socket _ -> liftEffect $ _send socket msg

runWsMsg :: WsMsgHandler -> WebSocket -> Foreign -> Effect Unit
runWsMsg (WsMsgHandlerM h) socket f = void $ launchAff_ $ h socket f

runWsReq :: WsReqHandler -> WebSocket -> Request -> Effect Unit
runWsReq (WsReqHandlerM h) socket req = void $ launchAff_ $ h socket req

onMessage :: WsMsgHandler -> WsReqHandler
onMessage h = WsReqHandlerM \socket _ ->
  liftEffect $ _onMessage socket $ runWsMsg h

ws :: forall r. RoutePattern r => r -> WsReqHandler -> App
ws r h = AppM \app -> runFn3 _ws app (unsafeToForeign r) $ runWsReq h


echo :: WsReqHandler
echo = WsReqHandlerM \socket req -> do
  liftEffect $ do
    log "connected"
    _onMessage socket $ runWsMsg $ WsMsgHandlerM \_ msg ->
      let realMsg = case read msg of
                      Right ms -> ms
                      Left err -> show err
      in liftEffect $ _send socket realMsg















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
