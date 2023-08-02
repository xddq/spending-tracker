-- for deriving ToRow
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- seems to be "desctructuring" from js/ts
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
  ( createPurchase,
    updatePurchase,
    deletePurchaseById,
    getPurchases,
    getPurchaseById,
    Purchase (Purchase, purchaseDate, purchasePriceInCent, purchaseTitle),
    CreatePurchaseInput (CreatePurchaseInput, createPurchaseInputTitle, createPurchaseInputPriceInCent, createPurchaseInputDate),
  )
where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import Data.Time (Day)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, execute, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)

-- for validating the purchase we pass when creating a new one
data CreatePurchaseInput = CreatePurchaseInput
  { createPurchaseInputTitle :: Text,
    createPurchaseInputPriceInCent :: Int,
    createPurchaseInputNameWhoPayed :: Text,
    createPurchaseInputDate :: Day
  }
  deriving (Show, Generic)

instance ToRow CreatePurchaseInput where
  -- NOTE: the order here (in the toRow on the right) determines the required
  -- order of args in the SQL insert statement.
  toRow CreatePurchaseInput {createPurchaseInputTitle, createPurchaseInputPriceInCent, createPurchaseInputNameWhoPayed, createPurchaseInputDate} = toRow (createPurchaseInputTitle, createPurchaseInputPriceInCent, createPurchaseInputNameWhoPayed, createPurchaseInputDate)

data Purchase = Purchase
  { purchaseId :: Int,
    purchaseTitle :: Text,
    purchasePriceInCent :: Int,
    purchaseNameWhoPayed :: Text,
    purchaseDate :: Day
  }
  deriving (Show, Generic, FromRow)

instance ToRow Purchase where
  -- NOTE: the order here (in the toRow on the right) determines the required
  -- order of args in the SQL insert statement.
  toRow Purchase {purchaseTitle, purchasePriceInCent, purchaseNameWhoPayed, purchaseDate, purchaseId} = toRow (purchaseTitle, purchasePriceInCent, purchaseNameWhoPayed, purchaseDate, purchaseId)

createPurchase :: Connection -> CreatePurchaseInput -> IO [Purchase]
createPurchase conn = query conn "INSERT INTO purchases (title, price_in_cent, name_who_payed, date) VALUES (?,?,?,?) RETURNING *"

updatePurchase :: Connection -> Purchase -> IO [Purchase]
updatePurchase conn = query conn "UPDATE purchases SET title=?, price_in_cent=?, name_who_payed=?, date=? WHERE id=? RETURNING *"

deletePurchaseById :: Connection -> Int -> IO Int64
deletePurchaseById conn purchaseId = execute conn "DELETE FROM purchases WHERE id = ?" (Only purchaseId)

getPurchases :: Connection -> IO [Purchase]
getPurchases conn = query_ conn "SELECT * FROM purchases ORDER BY date DESC"

getPurchaseById :: Connection -> Int -> IO [Purchase]
getPurchaseById conn pId = query conn "SELECT * FROM purchases WHERE id = ?" (Only pId)
