-- gives ability to specify data of type Text with "anythinghere" instead of
-- only String
{-# LANGUAGE OverloadedStrings #-}

module Views
  ( errorPage,
    landingPage,
    htmlToText,
    CurrentDate,
    mkCurrentDate,
    makeHtmlHead,
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

bulma :: Html
bulma = docTypeHtml $ do
  H.head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title "Hello Bulma!"
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
  body $ section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Hello World"
    p ! class_ "subtitle" $ do
      "My first website with"
      strong "Bulma"
      "!"

newtype Title = Title String

instance Show Title where
  show (Title x) = x

mkTitle :: String -> Title
mkTitle = Title

-- snippets

{-
 - Used to creat the html head for each page. Adds required metadata, loads
 - bulma css and sets the page title.
 -}
makeHtmlHead :: Title -> Html
makeHtmlHead x =
  H.head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title $ toHtml $ show x
    link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"

-- html snippet for adding a new purchase
addPurchaseSnippet :: CurrentDate -> Html
addPurchaseSnippet currentDate = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Eintrag hinzufügen"
    H.form ! target "_self" ! action "/api/add-entry" ! method "post" $ do
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Beschreibung des Einkaufs"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "title" ! required ""
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Wer hat gezahlt"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "whoPayed" ! required ""
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Preis in Euro (Z.b. 5.30 für 5.30€)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "number" ! name "priceInEuro" ! placeholder "1.0" ! step "0.01" ! A.min "0" ! required ""
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Datum (Korrektes Format ist z.b. 26.03.2023 oder 26.3.2023)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "(0[1-9]|[1-2][0-9]|3[0-1])\\.(0[1-9]|1[0-2])\\.20[0-9]{2}" ! placeholder (toValue $ show currentDate) ! required ""
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Eintrag erstellen"

bulmaCurr :: Html
bulmaCurr = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Liste aller Einkäufe"
    table ! class_ "table" $ do
      thead $ tr $ do
        th "Beschreibung des Einkaufs"
        th "Person die gezahlt hat"
        th "Preis in Euro"
        th "Datum des Einkaufs"
      tbody $ do
        tr $ do
          td "1"
          td "2"
          td "3"
          td "4"
        tr $ do
          td "1"
          td "2"
          td "3"
          td "4"

-- __snippets__
displayPurchases :: CurrentDate -> [Purchase] -> Html
displayPurchases x purchases = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Liste aller Einkäufe"
    table ! class_ "table" $ do
      thead $ tr $ do
        th "Beschreibung des Einkaufs"
        th "Person die gezahlt hat"
        th "Preis in Euro"
        th "Datum des Einkaufs"
      tbody $ mapM_ displayPurchase purchases

displayPurchase :: Purchase -> Html
displayPurchase (Purchase pId pTitle pPriceCent pWhoPayed pDate) = do
  H.tr $ do
    H.td $ toHtml pTitle
    H.td $ toHtml $ priceCentToEuroString pPriceCent
    H.td $ toHtml pWhoPayed
    H.td $ toHtml $ show $ mkCurrentDate pDate

-- pages
landingPage :: CurrentDate -> [Purchase] -> Html
landingPage x purchases = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Spending Tracker"
  body $ do
    addPurchaseSnippet x
    displayPurchases x purchases

errorPage :: Text -> Html
errorPage err = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Spending Tracker"
  body $ do
    p "An error occured:"
    br
    p $ toHtml err

-- __pages__

priceCentToEuroString :: Int -> String
priceCentToEuroString x =
  let price = show ((fromIntegral x) / 100)
   in if charactersAfterDot price == 1 then price ++ "0 €" else price ++ " €"

-- TODO: perhaps either use maybe or throw exception on 0. should never be 0.
charactersAfterDot :: String -> Int
charactersAfterDot str =
  case dropWhile (/= '.') str of
    [] -> 0 -- If there is no dot, return 0.
    dotStr -> length (tail dotStr) -- Get the length of characters after the dot.

htmlToText :: Html -> Text
htmlToText = renderHtml
