{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, decode, encode, fromJSON, object, withObject, (.:), (.=))
import Data.Maybe (listToMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.Read (decimal)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Database (CreatePurchaseInput (CreatePurchaseInput), Purchase (Purchase), createPurchase, getPurchases)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Views (CurrentDateAsString, displayPurchases, errorView, htmlToText, mkCurrentDateAsString, numbers)
import Web.Scotty (ActionM, body, delete, get, middleware, param, patch, post, scottyApp, setHeader, status, text)

newtype ApiError = ApiError {apiErrrorMessage :: Text}
  deriving (Show)

instance ToJSON ApiError where
  toJSON (ApiError msg) = object ["message" .= msg]

sendError :: Text -> Status -> ActionM ()
sendError message responseStatus = do
  setHeader "Content-Type" "application/json"
  status responseStatus
  text $ decodeUtf8 $ encode $ ApiError message

sendSuccess :: Text -> ActionM ()
sendSuccess message = do
  setHeader "Content-Type" "application/json"
  status status200
  text message

-- TODO: why is this signature not working?
-- allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PATCH", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }

-- days may have 1 or 2 chars, then one space, then month with one or two
-- letters then space and a 4 char year. Example: 23 07 2023
dateFormat :: String
dateFormat = "%-d %-m %Y"

textToDate :: String -> Maybe Day
textToDate = parseTimeM True defaultTimeLocale dateFormat

euroToCent :: Double -> Int
euroToCent x = round $ x * 100

-- returns the current date in the format we use for the app
getCurrentDateAsString :: IO CurrentDateAsString
getCurrentDateAsString = do
  timeNow <- utctDay <$> getCurrentTime
  return $ mkCurrentDateAsString $ formatTime defaultTimeLocale dateFormat timeNow

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev
    middleware allowCors

    -- expecting the form params here in order to create the new entry
    post "/api/add-entry" $ do
      titleValue <- param "title" :: ActionM Text
      priceInEuro <- param "priceInEuro" :: ActionM Double
      whoPayed <- param "whoPayed" :: ActionM Text
      date <- param "date" :: ActionM String
      case textToDate date of
        Nothing -> text $ htmlToText $ errorView "could not parse the given date"
        Just date -> do
          _purchase <- liftIO $ createPurchase conn (CreatePurchaseInput titleValue (euroToCent priceInEuro) whoPayed date)
          text "ok"

    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      purchases <- liftIO (getPurchases conn)
      currentDate <- liftIO getCurrentDateAsString
      text $ htmlToText $ displayPurchases currentDate purchases

    get "/numbers" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      status status200
      text $ htmlToText $ numbers 3
