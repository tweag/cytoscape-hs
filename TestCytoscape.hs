{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Lucid
import Lucid.Bootstrap3
import Graphics.Cytoscape

main :: IO ()
main = do
  T.writeFile "cytoscapetest.html" $ renderText testpage

testpage =  doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnJqueryJS
    cdnBootstrapJS
    cytoscapeCDN

  body_ $ do
     "Hello World"
     div_ [id_ "cy"] ""
     script_ $ runCytoscape nodes edges (Layout "cose") styles "" "#cy"
     style_ "#cy { height: 600px; width: 600px}"

nodes = [Node $ NodeData "a" "foo", Node $ NodeData "b" "bar"]

edges = [Edge $ EdgeData "a1" "a" "b"]

styles = [Style "node" [("color","black"),("content","data(name)")], Style "edge" []]
