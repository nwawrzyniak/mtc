module FrontendMain where

import Prelude hiding (apply, append)
import Effect (Effect)
import Effect.Console (log)
import JQuery (JQuery, JQueryEvent, ready, select, on, preventDefault, create
              , clear, clone, append, setHtml, setText, setProp, setValue)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)
import SimpleJquery.SimpleJquery (HTTPMethod(..), ajax, getKeycode, isShiftDown
                                  , serialize, trigger)
import Simple.JSON (read)
import Types (Msg, opSucceded)


main :: Effect Unit
main = ready do
  container <- select "#msgs"
  form      <- select "#form"
  textarea  <- select "#msgfield"
  msgTpl    <- create "<div class='msg'></div>"
  on "keypress" (handleKeypress form) textarea
  on "submit" (handleFormSubmit textarea) form
  initialLoadMsgs container msgTpl

handleKeypress :: JQuery -> JQueryEvent -> JQuery -> Effect Unit
handleKeypress form event _ = do
  keycode <- getKeycode event
  shiftDown <- isShiftDown event
  when (keycode == 13 && not shiftDown) do
    preventDefault event
    trigger "submit" form

handleFormSubmit :: JQuery -> JQueryEvent -> JQuery -> Effect Unit
handleFormSubmit textarea event form = do
  preventDefault event
  formData <- serialize form
  setProp "disabled" true textarea
  ajax "/api/msg" POST (Just formData) $ \a ->
    case read a of
      Right result -> do
        setProp "disabled" false textarea
        case unit of
          _ | result == opSucceded -> setValue "" textarea
            | otherwise            -> pure unit
      Left e -> log $ "deserialize failed: " <> show e

initialLoadMsgs :: JQuery -> JQuery -> Effect Unit
initialLoadMsgs container msgTpl = do
  clear container
  setText "Loading messages" container
  loadMsgs' Nothing \msgs -> do
    clear container
    _ <- flip traverse msgs $ \msg -> do
      tpl <- clone msgTpl
      setHtml msg.msg tpl
      append tpl container
    pure unit


loadMsgs' :: forall a. Maybe a -> (Array Msg -> Effect Unit) -> Effect Unit
loadMsgs' mData cb = do
  ajax "/api/get" GET mData $ \a ->
    case read a of
      Right (msgs :: Array Msg) -> cb msgs
      Left e -> log $ "deserialize failed: " <> show e
