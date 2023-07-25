{-# LANGUAGE OverloadedStrings #-}

module Views
  ( numbers,
    htmlToText,
    errorView,
    displayPurchases,
    CurrentDateAsString (CurrentDateAsString),
    mkCurrentDateAsString,
  )
where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import Database (Purchase (Purchase))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

numbers :: Int -> Html
numbers n = docTypeHtml $ do
  H.head $ do
    H.title "Natural numbers"
  body $ do
    p "A list of natural numbers:"
    ul $ forM_ [1 .. n] (li . toHtml)

newtype CurrentDateAsString = CurrentDateAsString String

mkCurrentDateAsString :: String -> CurrentDateAsString
mkCurrentDateAsString = CurrentDateAsString

-- html snippet for adding a new purchase
addPurchaseForm :: CurrentDateAsString -> Html
addPurchaseForm (CurrentDateAsString currentDate) = docTypeHtml $ do
  H.form ! target "_self" ! action "/api/add-entry" ! method "post" $ do
    H.label $ do
      "Beschreibung des Einkaufs"
      input ! type_ "text" ! name "title" ! required ""
    br
    H.label $ do
      "Wer hat gezahlt"
      input ! type_ "text" ! name "whoPayed" ! required ""
      br
    H.label $ do
      "Preis in Euro (beispiel 5.30 für 5.30€)"
      input ! type_ "number" ! name "priceInEuro" ! placeholder "1.0" ! step "0.01" ! A.min "0" ! required ""
    br
    H.label $ do
      "Datum (Mit Leerzeichen. Korrekt ist z.b. 26 03 2023)"
      input ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "(0[1-9]|[1-2][0-9]|3[0-1]) (0[1-9]|1[0-2]) [0-9]{4}" ! placeholder (toValue currentDate) ! required ""
    br
    input ! type_ "submit" ! value "Eintrag erstellen"

displayPurchases :: CurrentDateAsString -> [Purchase] -> Html
displayPurchases x purchases = docTypeHtml $ do
  H.head $ H.title "Purchases"
  body $ do
    h1 "Eintrag hinzufügen"
    addPurchaseForm x
    br
    h1 "Liste aller Einträge"
    ul $ mapM_ displayPurchase purchases

displayPurchase :: Purchase -> Html
displayPurchase (Purchase pId pTitle pPriceCent pWhoPayed pDate) = do
  li $ do
    H.div $ do
      toHtml pTitle
      preEscapedText "&nbsp;&nbsp;&nbsp;&nbsp;"
      toHtml $ priceCentToEuroString pPriceCent
      preEscapedText "&nbsp;&nbsp;&nbsp;&nbsp;"
      toHtml pWhoPayed
      preEscapedText "&nbsp;&nbsp;&nbsp;&nbsp;"
      toHtml $ show pDate

errorView :: Text -> Html
errorView err = docTypeHtml $ do
  H.head $ H.title "Error"
  body $ do
    p "An error occured:"
    br
    p $ toHtml err

priceCentToEuroString :: Int -> String
priceCentToEuroString x = show ((fromIntegral x) / 100) ++ "€"

htmlToText :: Html -> Text
htmlToText = renderHtml
