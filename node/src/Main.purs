module Main where

import Prelude hiding (apply)
import Data.Maybe (fromMaybe)
import Data.Int (fromString)
import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App, listenHttp, get)
import Node.Express.Response (send)
import Node.HTTP (Server)
import Node.Process (lookupEnv)


parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


app :: App
app = get "/" $ send "Hello, World!"

main :: Effect Server
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp app port \_ ->
      log $ "Listening on " <> show port
