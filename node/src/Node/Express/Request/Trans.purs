module Node.Express.Request.Trans
( getRouteParam, getQueryParam, getQueryParams, getBody, getBody'
  , getBodyParam, getRoute
  , getCookie, getSignedCookie
  , getRequestHeader
  , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
  , getRemoteIp, getRemoteIps, getPath, getHostname, getSubdomains
  , isFresh, isStale
  , isXhr, getProtocol, getMethod
  , getUrl, getOriginalUrl
  , getUserData, setUserData
  ) where

import Prelude
import Data.Maybe (Maybe)
import Foreign (Foreign, F)
import Foreign.Class (class Decode)
import Node.Express.Handler (Handler)
import Node.Express.Handler.Trans (class MonadHandler, liftHandler)
import Node.Express.Request (getRouteParam, getQueryParam, getQueryParams, getBody, getBody'
  , getBodyParam, getRoute
  , getCookie, getSignedCookie
  , getRequestHeader
  , accepts, ifAccepts, acceptsCharset, acceptsLanguage, hasType
  , getRemoteIp, getRemoteIps, getPath, getHostname, getSubdomains
  , isFresh, isStale
  , isXhr, getProtocol, getMethod
  , getUrl, getOriginalUrl
  , getUserData, setUserData) as H
import Node.Express.Types (class RequestParam, Protocol, Method)

getRouteParam :: forall m a. MonadHandler m => RequestParam a => a -> m (Maybe String)
getRouteParam = liftHandler <<< H.getRouteParam

getQueryParam :: forall m a. MonadHandler m => String -> m (Maybe a)
getQueryParam = liftHandler <<< H.getQueryParam

getQueryParams :: forall m a. MonadHandler m => String -> m (Maybe (Array a))
getQueryParams = liftHandler <<< H.getQueryParams

getBody :: forall m a. MonadHandler m => Decode a => m (F a)
getBody = liftHandler H.getBody

getBody' :: forall m. MonadHandler m => m Foreign
getBody' = liftHandler H.getBody'

getBodyParam :: forall m a. MonadHandler m => String -> m (Maybe a)
getBodyParam = liftHandler <<< H.getBodyParam

getRoute :: forall m. MonadHandler m => m String
getRoute = liftHandler H.getRoute

getCookie :: forall m. MonadHandler m => String -> m (Maybe String)
getCookie = liftHandler <<< H.getCookie

getSignedCookie :: forall m. MonadHandler m => String -> m (Maybe String)
getSignedCookie = liftHandler <<< H.getSignedCookie

getRequestHeader :: forall m. MonadHandler m => String -> m (Maybe String)
getRequestHeader = liftHandler <<< H.getRequestHeader

accepts :: forall m. MonadHandler m => String -> m (Maybe String)
accepts = liftHandler <<< H.accepts

ifAccepts :: forall m. MonadHandler m => String -> Handler -> m Unit
ifAccepts = (liftHandler <<< _) <<< H.ifAccepts

acceptsCharset :: forall m. MonadHandler m => String -> m (Maybe String)
acceptsCharset = liftHandler <<< H.acceptsCharset

acceptsLanguage :: forall m. MonadHandler m => String -> m (Maybe String)
acceptsLanguage = liftHandler <<< H.acceptsLanguage

hasType :: forall m. MonadHandler m => String -> m Boolean
hasType = liftHandler <<< H.hasType

getRemoteIp :: forall m. MonadHandler m => m String
getRemoteIp = liftHandler H.getRemoteIp

getRemoteIps :: forall m. MonadHandler m => m (Array String)
getRemoteIps = liftHandler H.getRemoteIps

getPath :: forall m. MonadHandler m => m String
getPath = liftHandler H.getPath

getHostname :: forall m. MonadHandler m => m String
getHostname = liftHandler H.getHostname

getSubdomains :: forall m. MonadHandler m => m (Array String)
getSubdomains = liftHandler H.getSubdomains

isFresh :: forall m. MonadHandler m => m Boolean
isFresh = liftHandler H.isFresh

isStale :: forall m. MonadHandler m => m Boolean
isStale = liftHandler H.isStale

isXhr :: forall m. MonadHandler m => m Boolean
isXhr = liftHandler H.isXhr

getProtocol :: forall m. MonadHandler m => m (Maybe Protocol)
getProtocol = liftHandler H.getProtocol

getMethod :: forall m. MonadHandler m => m Method
getMethod = liftHandler H.getMethod

getUrl :: forall m. MonadHandler m => m String
getUrl = liftHandler H.getUrl

getOriginalUrl :: forall m. MonadHandler m => m String
getOriginalUrl = liftHandler H.getOriginalUrl

getUserData :: forall m a. MonadHandler m => String -> m (Maybe a)
getUserData = liftHandler <<< H.getUserData

setUserData :: forall m a. MonadHandler m => String -> a -> m Unit
setUserData = (liftHandler <<< _) <<< H.setUserData
