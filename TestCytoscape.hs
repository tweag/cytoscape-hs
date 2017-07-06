{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Lucid
import Graphics.Cytoscape

main :: IO ()
main = do
  T.writeFile "cytoscapetest.html" $ renderText testpage

testpage =  doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cytoscapeCDN

  body_ $ do
     "Hello World"
