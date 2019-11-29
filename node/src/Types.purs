module Types ( Timestamp(..)
             , Msg(..)
             , RawMsg
             , OperationStatus
             , instantToTimestamp
             , opSucceded
             , opFailed
             ) where

import Prelude hiding (apply)
import Data.Int (floor)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.DateTime.Instant (Instant, unInstant)
import Simple.JSON ( class ReadForeign , readImpl
                   , class WriteForeign, writeImpl
                   )

newtype Timestamp = Timestamp Int

derive instance eqTimestamp :: Eq Timestamp
derive instance newtypeTimestamp :: Newtype Timestamp _
instance showTimestamp :: Show Timestamp where
  show (Timestamp t) = "Timestamp " <> show t

instance readTs :: ReadForeign Timestamp where
  readImpl a = do
    b <- readImpl a
    pure $ Timestamp b

instance writeTs :: WriteForeign Timestamp where
  writeImpl (Timestamp a) = writeImpl a

instantToTimestamp :: Instant -> Timestamp
instantToTimestamp = wrap <<< floor <<< (flip div 1000.0) <<< unwrap <<< unInstant

type Msg = { msg       :: String
           , timestamp :: Timestamp
           }





type RawMsg = {"msg" :: String }

type OperationStatus = {"success" :: Boolean}

opSucceded :: OperationStatus
opSucceded = {success: true}

opFailed :: OperationStatus
opFailed = {success: false}

{-
instance readMsg :: ReadForeign Msg where
  readImpl = do
    s <- readImpl
    (i :: Number) <- readImpl
    case (instant $ Milliseconds i) of
      Just is -> pure $ Msg {msg: s, inst: Milliseconds is}
      Nothing -> fail $ ForeignError "could not read foreign timestamp"

-}
