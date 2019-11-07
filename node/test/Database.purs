module Test.Database where

import Prelude hiding (apply)

import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.FS.Aff (exists, unlink)
import SQLite3 (closeDB, newDB, queryDB, queryObjectDB)
import Simple.JSON (read)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)


import Types
import Database



testTime :: Timestamp
testTime = Timestamp 9999999

testMsg :: String
testMsg = "test message"

main :: Effect Unit
main = launchAff_ do
  let testPath = "./data/test.sqlite3"
  (flip when) (unlink testPath) =<< exists testPath
  db <- newDB testPath
  _ <- queryDB db sqlCreateTableIfNotExists []
  
  let prepedDb = prepareDb db


  liftEffect $ runTest do
    suite "Database Tests" do

      test ("db connection worked and created " <> testPath) do
        assert "exists testPath" =<< exists testPath
        
      test "we can insert rows and retrieve them" do
        let message = {msg: testMsg, timestamp: testTime }
        _ <- prepedDb $ sqlInsertMessage message
        results <- read <$> prepedDb sqlGetMessages
        case results of
          Right (as :: Array Msg) ->
            for_ as \(a) -> do
              equal a.msg testMsg
              equal a.timestamp testTime
          Left e ->
            failure $ "row didn't deserialize correctly: " <> show e
