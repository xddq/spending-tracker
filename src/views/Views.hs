-- gives ability to specify data of type Text with "anythinghere" instead of
-- only String
{-# LANGUAGE OverloadedStrings #-}

module Views
  ( htmlToText,
    errorView,
    displayPurchases,
    CurrentDate,
    mkCurrentDate,
  )
where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Database (Purchase (Purchase))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

newtype CurrentDate = CurrentDate Day

mkCurrentDate :: Day -> CurrentDate
mkCurrentDate = CurrentDate

instance Show CurrentDate where
  -- days may have 1 or 2 chars, then one space, then month with one or two
  -- letters then space and a 4 char year. Example: 23.07.2023
  show (CurrentDate a) = formatTime defaultTimeLocale "%-d.%-m.%Y" a

-- html snippet for adding a new purchase
addPurchaseForm :: CurrentDate -> Html
addPurchaseForm currentDate = docTypeHtml $ do
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
      input ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "(0[1-9]|[1-2][0-9]|3[0-1])\\.(0[1-9]|1[0-2])\\.20[0-9]{2}" ! placeholder (toValue $ show currentDate) ! required ""
    br
    input ! type_ "submit" ! value "Eintrag erstellen"

displayPurchases :: CurrentDate -> [Purchase] -> Html
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
      toHtml $ show $ mkCurrentDate pDate

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

-- numbers :: Int -> Html
-- numbers n = docTypeHtml $ do
--   H.head $ do
--     H.title "Natural numbers"
--   body $ do
--     p "A list of natural numbers:"
--     ul $ forM_ [1 .. n] (li . toHtml)
