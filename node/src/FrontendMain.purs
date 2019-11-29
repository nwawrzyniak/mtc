module FrontendMain where

import Prelude hiding (apply)
import Effect (Effect)
import Effect.Console (log)
--import Effect.Class (liftEffect)
import JQuery (JQuery, JQueryEvent, ready, select, on, preventDefault)
--import Affjax as AX
--import Affjax.ResponseFormat as ResponseFormat
--import Affjax.RequestBody as RequestBody
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
--import Effect.Aff (launchAff_)
import SimpleJquery.SimpleJquery (HTTPMethod(..), ajax, getKeycode, isShiftDown
                                  , serialize, trigger)
import Simple.JSON (read)
import Types (Msg, opSucceded)



main :: Effect Unit
main = ready do
  container <- select "#msgs"
  form      <- select "#form"
  textarea  <- select "#msgfield"
  on "keypress" (handleKeypress form) textarea
  on "submit" (handleFormSubmit textarea) form

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
  ajax "/api/msg" POST (Just formData) $ \a ->
    case read a of
      Right result | result == opSucceded -> log "a"
                   | otherwise            -> log "b"
      Left e -> log $ "deserialize failed: " <> show e
  --launchAff_ do
  --  result <- AX.post ResponseFormat.json "/api/msg" $ Just $ RequestBody.string formData
  --  case result of
  --    Left err -> liftEffect $ log $ AX.printError err
  --    Right response -> liftEffect $ log $ response.statusText
