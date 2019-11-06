module Types where

import Prelude hiding (apply)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), fail)
import Data.DateTime.Instant (Instant, instant)
import Data.Time.Duration (Milliseconds(..))
import Simple.JSON (class ReadForeign, readImpl)

newtype Timestamp = Timestamp Int

derive instance eqTimestamp :: Eq Timestamp
instance showTimestamp :: Show Timestamp where
  show (Timestamp t) = "Timestamp " <> show t

type Msg = { msg       :: String
           , timestamp :: Timestamp
           }

unTimestamp :: Timestamp -> Int
unTimestamp (Timestamp t) = t


instance readMsg :: ReadForeign Timestamp where
  readImpl a = do
    b <- readImpl a
    pure $ Timestamp b

{-
instance readMsg :: ReadForeign Msg where
  readImpl = do
    s <- readImpl
    (i :: Number) <- readImpl
    case (instant $ Milliseconds i) of
      Just is -> pure $ Msg {msg: s, inst: Milliseconds is}
      Nothing -> fail $ ForeignError "could not read foreign timestamp"

-}
