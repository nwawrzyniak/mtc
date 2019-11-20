module FrontendMain where

import Prelude hiding (apply)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import JQuery (JQuery, JQueryEvent, ready, select, on, preventDefault)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff (launchAff_)

foreign import trigger :: String -> JQuery -> Effect Unit
foreign import getKeycode :: JQueryEvent -> Effect Int
foreign import isShiftDown :: JQueryEvent -> Effect Boolean
foreign import serialize :: JQuery -> Effect String

main :: Effect Unit
main = ready do
  container <- select "#msgs"
  form <- select "#form"
  textarea <- select "#msgfield"
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
  launchAff_ do
    result <- AX.post ResponseFormat.json "/api/msg" $ Just $ RequestBody.string formData
    case result of
      Left err -> liftEffect $ log $ AX.printError err
      Right response -> liftEffect $ log $ response.statusText
