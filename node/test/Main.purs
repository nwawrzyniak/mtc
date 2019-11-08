module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Database (main) as DB

main :: Effect Unit
main = do
  log "You should add some tests."
  DB.main
