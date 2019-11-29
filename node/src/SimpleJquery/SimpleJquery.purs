module SimpleJquery.SimpleJquery
  ( HTTPMethod(..)
  , URL
  , trigger
  , getKeycode
  , isShiftDown
  , serialize
  , ajax
  ) where


import Prelude hiding (apply)
import JQuery (JQuery, JQueryEvent)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)

data HTTPMethod = GET | POST

type URL = String

foreign import trigger :: String -> JQuery -> Effect Unit
foreign import getKeycode :: JQueryEvent -> Effect Int
foreign import isShiftDown :: JQueryEvent -> Effect Boolean
foreign import serialize :: JQuery -> Effect String
foreign import _ajax :: forall a. a -> Effect Unit

ajax :: forall a.
        URL
     -> HTTPMethod
     -> Maybe a
     -> (Foreign -> Effect Unit)
     -> Effect Unit
ajax url method (Just datum) cb =
  _ajax  { url: url
         , method: methodToString method
         , data: datum
         , success: cb
         }
ajax url method Nothing cb =
  _ajax  { url: url
         , method: methodToString method
         , success: cb
         }

methodToString :: HTTPMethod -> String
methodToString GET  = "get"
methodToString POST = "post"
