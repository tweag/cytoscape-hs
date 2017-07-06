{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Graphics.Cytoscape where

import Data.Aeson
import Lucid


cytoscapeCDN :: Monad m => HtmlT m ()
cytoscapeCDN = script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.1.2/cytoscape.min.js"] ""
