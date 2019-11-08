module Main where

import Prelude hiding (apply)
import Data.Maybe (fromMaybe)
import Data.Int (fromString)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Node.Express.App (App, listenHttp, get, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Response (send, sendJson, setStatus)
import Node.Express.Middleware.Static (static)
import Node.HTTP (Server)
import Node.Process (lookupEnv)


parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str


errorHandler :: Error -> Handler
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

app :: App
app = do
    let static' = static "./static/"
    get "/"          $ static'
    get "/style.css" $ static'
    get "/hello"     $ send "Hello, World!"
    useOnError       $ errorHandler

main :: Effect Server
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp app port \_ ->
      log $ "Listening on " <> show port
