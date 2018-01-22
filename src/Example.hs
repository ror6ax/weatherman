{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello, this is today's weather. Use /weather endpoint!"

  S.get "/weather" $ do 
    S.json $ object ["temperature" .= Number 23, "verbal description" .= String "not Hawaii"]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'
