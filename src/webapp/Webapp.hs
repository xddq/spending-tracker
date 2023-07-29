{-# LANGUAGE OverloadedStrings #-}

module Webapp (mkApp) where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), ToJSON (toJSON), Value, decode, encode, fromJSON, object, withObject, (.:), (.=))
import Data.Maybe (listToMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text, pack, unpack)
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
import Views (errorPage, htmlToText, landingPage, mkCurrentDate)
import Web.Scotty (ActionM, body, delete, get, middleware, param, patch, post, redirect, scottyApp, setHeader, status, text)

-- days may have 1 or 2 chars, then one space, then month with one or two
-- letters then space and a 4 char year. Example: 23 07 2023
dateFormat :: String
dateFormat = "%-d.%-m.%Y"

textToDate :: String -> Maybe Day
textToDate = parseTimeM True defaultTimeLocale dateFormat

euroToCent :: Double -> Int
euroToCent x = round $ x * 100

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    -- expecting the form params here in order to create the new entry
    post "/api/add-entry" $ do
      titleValue <- param "title" :: ActionM Text
      priceInEuro <- param "priceInEuro" :: ActionM Double
      whoPayed <- param "whoPayed" :: ActionM Text
      date <- param "date" :: ActionM String
      case textToDate date of
        Nothing -> text $ htmlToText $ errorPage "could not parse the given date"
        Just date -> do
          _purchase <- liftIO $ createPurchase conn (CreatePurchaseInput titleValue (euroToCent priceInEuro) whoPayed date)
          redirect "/"

    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      purchases <- liftIO (getPurchases conn)
      currentDate <- liftIO (utctDay <$> getCurrentTime)
      text $ htmlToText $ landingPage (mkCurrentDate currentDate) purchases

    get "/purchases/:id/edit" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> text $ htmlToText (errorPage $ pack err)
        Right (parsedId, _rest) -> do
          -- TODO - continue here:
          --   - Implement getPurchaseById
          --   - pass purchase to edit page
          deletedRowsCount <- liftIO (deleteTodo conn parsedId)
          editPurchasePage

-- deletedRowsCount <- liftIO (deleteTodo conn parsedId)
-- if deletedRowsCount == 1
--   then sendSuccess $ decodeUtf8 $ encode $ object ["message" .= ("ok" :: Text)]
--   else sendError (mkApiError "not found") status404

-- get "/numbers" $ do
--   setHeader "Content-Type" "text/html; charset=utf-8"
--   status status200
--   text $ htmlToText $ numbers 3
