-- gives ability to specify data of type Text with "anythinghere" instead of
-- only String
{-# LANGUAGE OverloadedStrings #-}

module Views
  ( errorPage,
    successPage,
    landingPage,
    editPurchasePage,
    deletePurchasePage,
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

newtype Title = Title String

instance Show Title where
  show (Title x) = x

mkTitle :: String -> Title
mkTitle = Title

-- delay for page redirect in seconds
newtype Delay = Delay Int

instance Show Delay where
  show (Delay x) = show x

mkDelay :: Int -> Delay
mkDelay = Delay

-- TODO/Maybe make data constructor for page and add all pages there..?
type Page = String

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
    link ! rel "stylesheet" ! href "/bulma-0.9.4.min.css"

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
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "([1-9]|[0-2][0-9]|3[0-1])\\.(0[1-9]|[1-9]|1[0-2])\\.20[0-9]{2}" ! value (toValue $ show currentDate) ! required ""
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Eintrag erstellen"

-- html snippet for editing a purchase
editPurchaseSnippet :: Purchase -> Html
editPurchaseSnippet (Purchase pId pTitle pPriceCent pWhoPayed pDate) = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Eintrag bearbeiten"
    H.form ! target "_self" ! action "/api/update-entry" ! method "post" $ do
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Beschreibung des Einkaufs"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "title" ! required "" ! value (toValue pTitle)
        input ! type_ "hidden" ! name "id" ! value (toValue pId)
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Wer hat gezahlt"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! name "whoPayed" ! required "" ! value (toValue pWhoPayed)
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Preis in Euro (Z.b. 5.30 für 5.30€)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "number" ! name "priceInEuro" ! value (toValue $ show ((fromIntegral pPriceCent) / 100)) ! step "0.01" ! A.min "0" ! required ""
      H.div ! class_ "field" $ do
        H.label ! class_ "label" $ "Datum (Korrektes Format ist z.b. 26.03.2023 oder 26.3.2023)"
        H.div ! class_ "control" $ input ! class_ "input" ! type_ "text" ! A.id "dateInput" ! name "date" ! pattern "([1-9]|[0-2][0-9]|3[0-1])\\.(0[1-9]|[1-9]|1[0-2])\\.20[0-9]{2}" ! value (toValue $ show $ mkCurrentDate pDate) ! required ""
      H.div ! class_ "field" $ H.div ! class_ "control" $ input ! class_ "button is-link" ! type_ "submit" ! value "Update speichern"

deletePurchaseSnippet :: Purchase -> Html
deletePurchaseSnippet (Purchase pId pTitle _ _ pDate) = do
  H.div ! class_ "section" $ H.div ! class_ "container" $ do
    H.div ! class_ "title is-4" $ "Eintrag löschen"
    H.div ! class_ "card" $ do
      H.div ! class_ "card-content" $ H.div ! class_ "content" $ do
        "Bist du dir sicher, dass du den Einkauf mit dem Titel \""
        strong $ toHtml pTitle
        "\" vom "
        strong $ toHtml $ show $ mkCurrentDate pDate
        " unwiderruflich löschen möchtest?"
      footer ! class_ "card-footer" $ do
        H.form ! class_ "card-footer-item" ! target "_self" ! action "/" ! method "get" $ do
          a ! class_ "card-footer-item button is-primary" ! href "/" $ "Zurück"
        H.form ! class_ "card-footer-item" ! target "_self" ! action "/api/delete-entry" ! method "post" $ do
          input ! class_ "card-footer-item button is-danger" ! type_ "hidden" ! name "id" ! value (toValue $ show pId)
          input ! class_ "card-footer-item button is-danger" ! type_ "submit" ! value "Löschen"

displayPurchasesListSnippet :: CurrentDate -> [Purchase] -> Html
displayPurchasesListSnippet x purchases = do
  section ! class_ "section" $ H.div ! class_ "container" $ do
    h1 ! class_ "title" $ "Liste aller Einkäufe"
    table ! class_ "table" $ do
      thead $ tr $ do
        th "Beschreibung des Einkaufs"
        th "Person die gezahlt hat"
        th "Preis in Euro"
        th "Datum des Einkaufs"
        th "Eintrag bearbeiten"
        th "Eintrag löschen"
      tbody $ mapM_ displayPurchaseListItemSnippet purchases

displayPurchaseListItemSnippet :: Purchase -> Html
displayPurchaseListItemSnippet (Purchase pId pTitle pPriceCent pWhoPayed pDate) = do
  H.tr $ do
    H.td $ toHtml pTitle
    H.td $ toHtml pWhoPayed
    H.td $ toHtml $ priceCentToEuroString pPriceCent
    H.td $ toHtml $ show $ mkCurrentDate pDate
    H.td $ a ! href (toValue $ "/purchases/" ++ show pId ++ "/edit") ! class_ "button is-primary" $ "Bearbeiten"
    H.td $ a ! href (toValue $ "/purchases/" ++ show pId ++ "/delete") ! class_ "button is-danger" $ "Löschen"

redirectToHomeSnippet :: Html
redirectToHomeSnippet = p $ do
  "Klicke"
  a ! href "/" $ "hier"
  "um zurück zur Hauptseite zu gelangen."

-- __snippets__

-- pages
landingPage :: CurrentDate -> [Purchase] -> Html
landingPage x purchases = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Spending Tracker"
  body $ do
    addPurchaseSnippet x
    displayPurchasesListSnippet x purchases

editPurchasePage :: Purchase -> Html
editPurchasePage purchase = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Edit Purchase"
  body $ editPurchaseSnippet purchase

deletePurchasePage :: Purchase -> Html
deletePurchasePage purchase = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Delete Purchase"
  body $ deletePurchaseSnippet purchase

successPage :: Text -> Html
successPage message = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Success"
  body $ section ! class_ "section" $ H.div ! class_ "container" $ H.div ! class_ "notification is-success subtitle is-5" $ do
    "Erfolgreich durgeführt! Klicke"
    a ! href "/" $ " hier "
    "um zurück zur Startseite zu gelangen."

errorPage :: Text -> Html
errorPage err = docTypeHtml $ do
  makeHtmlHead $ mkTitle "Error"
  body $ section ! class_ "section" $ H.div ! class_ "container" $ H.div ! class_ "notification is-danger subtitle is-5" $ do
    "Fehler beim ausführen! Klicke"
    a ! href "/" $ " hier "
    "um zurück zur Startseite zu gelangen."

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
