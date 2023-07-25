{-# LANGUAGE OverloadedStrings #-}

module Views
  ( numbers,
    htmlToText,
  )
where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
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

htmlToText :: Html -> Text
htmlToText = renderHtml
