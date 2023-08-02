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
import Database (CreatePurchaseInput (CreatePurchaseInput), Purchase (Purchase, purchasePriceInCent, purchaseTitle), createPurchase, deletePurchaseById, getPurchaseById, getPurchases, updatePurchase)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Text.Blaze.Html (Html)
import Views (deletePurchasePage, editPurchasePage, errorPage, htmlToText, landingPage, mkCurrentDate, successPage)
import Web.Scotty (ActionM, body, delete, get, html, middleware, param, patch, post, redirect, scottyApp, setHeader, status, text)

-- days may have 1 or 2 chars, then one space, then month with one or two
-- letters then space and a 4 char year. Example: 23 07 2023
dateFormat :: String
dateFormat = "%-d.%-m.%Y"

textToDate :: String -> Maybe Day
textToDate = parseTimeM True defaultTimeLocale dateFormat

euroToCent :: Double -> Int
euroToCent x = round $ x * 100

displayPage :: Html -> ActionM ()
displayPage x = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  text $ htmlToText x

mkApp :: Connection -> IO Application
mkApp conn =
  scottyApp $ do
    -- Add any WAI middleware, they are run top-down.
    -- log all requests in console
    middleware logStdoutDev
    -- serve static files from the "static" directory
    middleware $ staticPolicy (addBase "static")

    get "/" $ do
      purchases <- liftIO (getPurchases conn)
      currentDate <- liftIO (utctDay <$> getCurrentTime)
      displayPage $ landingPage (mkCurrentDate currentDate) purchases

    get "/purchases/:id/edit" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> text $ htmlToText (errorPage $ pack err)
        Right (parsedId, _rest) -> do
          purchaseList <- liftIO (getPurchaseById conn parsedId)
          case listToMaybe purchaseList of
            Just purchase -> displayPage $ editPurchasePage purchase
            Nothing -> displayPage $ errorPage "not found"

    get "/purchases/:id/delete" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> text $ htmlToText (errorPage $ pack err)
        Right (parsedId, _rest) -> do
          purchaseList <- liftIO (getPurchaseById conn parsedId)
          case listToMaybe purchaseList of
            Just purchase -> displayPage $ deletePurchasePage purchase
            Nothing -> displayPage $ errorPage "not found"

    -- expecting the form params here in order to create the new entry
    post "/api/add-entry" $ do
      titleValue <- param "title" :: ActionM Text
      priceInEuro <- param "priceInEuro" :: ActionM Double
      whoPayed <- param "whoPayed" :: ActionM Text
      date <- param "date" :: ActionM String
      case textToDate date of
        Nothing -> displayPage $ errorPage "could not parse the given date"
        Just date -> do
          _purchase <- liftIO $ createPurchase conn (CreatePurchaseInput titleValue (euroToCent priceInEuro) whoPayed date)
          displayPage $ successPage "purchase was created successfully"

    post "/api/update-entry" $ do
      purchaseId <- param "id" :: ActionM Int
      titleValue <- param "title" :: ActionM Text
      priceInEuro <- param "priceInEuro" :: ActionM Double
      whoPayed <- param "whoPayed" :: ActionM Text
      date <- param "date" :: ActionM String
      -- TODO: how can we write these two "case .. of, case .. of 'cleaner'?"
      case textToDate date of
        Nothing -> displayPage $ errorPage "could not parse the given date"
        Just date -> do
          updatedPurchase <- liftIO $ updatePurchase conn (Purchase purchaseId titleValue (euroToCent priceInEuro) whoPayed date)
          case listToMaybe updatedPurchase of
            Nothing -> displayPage $ errorPage "not found"
            Just x -> displayPage $ successPage "purchase was updated successfully"

    post "/api/delete-entry" $ do
      unparsedId <- param "id"
      case decimal unparsedId of
        Left err -> text $ htmlToText (errorPage $ pack err)
        Right (purchaseId, _) -> do
          deletedRowsCount <- liftIO $ deletePurchaseById conn purchaseId
          if deletedRowsCount == 1
            then displayPage $ successPage "purchase was deleted successfully"
            else displayPage $ errorPage "not found"
