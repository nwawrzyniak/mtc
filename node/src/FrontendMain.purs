module FrontendMain where

import Prelude hiding (apply, append)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Either (Either(..))
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Timer (setTimeout)
import Effect.Ref as Ref
import JQuery (JQuery, JQueryEvent, ready, select, on, preventDefault, create
              , clear, clone, append, setHtml, setText, setProp, setValue)
import Simple.JSON (read)

import Types (Msg, RawTimestamp, instantToTimestamp, opSucceded)
import SimpleJquery.SimpleJquery (HTTPMethod(..), ajax, getKeycode, isShiftDown
                                  , serialize, trigger)

main :: Effect Unit
main = ready do
  container <- select "#msgs"
  form      <- select "#form"
  textarea  <- select "#msgfield"
  msgTpl    <- create "<div class='msg'></div>"
  on "keypress" (handleKeypress form) textarea
  on "submit" (handleFormSubmit textarea) form
  initialLoadMsgs container msgTpl

-- | Handles a keypress on the textarea.
-- | First argument is the form. Other arguments are filled by the `on`
-- | function.
-- | This is used to submit the form when `enter` (but not `shift + enter` is
-- | pressed on the textarea
handleKeypress :: JQuery -> JQueryEvent -> JQuery -> Effect Unit
handleKeypress form event _ = do
  keycode <- getKeycode event
  shiftDown <- isShiftDown event
  when (keycode == 13 && not shiftDown) do
    preventDefault event
    trigger "submit" form

-- | Handles form submission. Instead of normally sending the form, this will
-- | send the form via ajax/xhr
handleFormSubmit :: JQuery -> JQueryEvent -> JQuery -> Effect Unit
handleFormSubmit textarea event form = do
  preventDefault event
  formData <- serialize form
  --setProp "disabled" true textarea
  setValue "" textarea
  ajax "/api/msg" POST (Just formData) $ \a ->
    case read a of
      Right result ->
--        setProp "disabled" false textarea
        case unit of
          _ | result == opSucceded -> pure unit
            | otherwise            -> log "failed to send message"
      Left e -> log $ "deserialize failed: " <> show e

-- | `Regex` to find `\n` or `\r` or combinations of the two
nl2brRegex :: Regex
nl2brRegex = unsafeRegex "(\r\n|\n\r|\r|\n)" global

-- | Adds a html `<br>` where a newline is
nl2br :: String -> String
nl2br = replace nl2brRegex "<br>$1"

-- | Add a message to the conatiner
-- | Arguments: Container to add message to, template to put text into, the message
addMsgJq :: JQuery -> JQuery -> Msg -> Effect Unit
addMsgJq container msgTpl msg = do
  tpl <- clone msgTpl
  setHtml (nl2br msg.msg) tpl
  append tpl container

-- | Load all messages on page startup, add them to the container and start the
-- | cronjob which pulls for new messages
initialLoadMsgs :: JQuery -> JQuery -> Effect Unit
initialLoadMsgs container msgTpl = do
  clear container
  setText "Loading messages" container
  loadMsgs Nothing \msgs -> do
    clear container
    _ <- flip traverse msgs $ addMsgJq container msgTpl
    loadMsgCronJob container msgTpl

-- | Pulls for new messages and adds them to the container
loadMsgCronJob :: JQuery -> JQuery -> Effect Unit
loadMsgCronJob container msgTpl = do
  initialTs <- unwrap <$> instantToTimestamp <$> now
  refTs <- Ref.new initialTs
  _ <- setTimeout repeatTime $ act refTs
  pure unit
  where
    repeatTime :: Int
    repeatTime = 1000
    act :: Ref.Ref Int -> Effect Unit
    act refTs = do
      lastTs <- Ref.read refTs
      loadMsgs (Just ({timestamp: show lastTs} :: RawTimestamp)) \msgs -> do
        _ <- flip traverse msgs $ addMsgJq container msgTpl
        nextTs <- unwrap <$> instantToTimestamp <$> now
        _ <- setTimeout repeatTime $ act refTs
        Ref.write nextTs refTs

-- | Wrapper around the `ajax` method handeling parsing of the messages
loadMsgs :: forall a. Maybe a -> (Array Msg -> Effect Unit) -> Effect Unit
loadMsgs mData cb = do
  let method = case mData of
              Just _  -> POST
              Nothing -> GET
  ajax "/api/get" method mData $ \a ->
    case read a of
      Right (msgs :: Array Msg) -> cb msgs
      Left e -> log $ "deserialize failed: " <> show e
