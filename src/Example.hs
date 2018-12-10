{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Example (runApp, app) where

import GHC.Generics
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S
import Web.Scotty.Trans (json, jsonData)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import Data.String.Interpolate
import Module1

data Place = Place { lon :: Int, lat :: Int } deriving (Show, Generic)

instance ToJSON Place
instance FromJSON Place

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    let x = Module1.function1 123
    S.text [i|hello, this is today's weather. Use /weather endpoint!: #{x}|]

  S.get "/weather" $ do
    S.json $ object ["temperature" .= Number 23, "verbal description" .= String "not Hawaii"]

  S.post "/weather1/:place" $ do
    place <- S.param "place"
    S.text ("hello " <> place <> "!")

  S.post "/weather2/:place" $ do
    place <- jsonData :: S.ActionM Place
    S.json place

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'
