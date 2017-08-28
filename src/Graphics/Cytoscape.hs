{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell, DeriveGeneric #-}

module Graphics.Cytoscape where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Lucid
import Lens.Micro.TH
import GHC.Generics

jsonOptions :: Maybe String -> Options
jsonOptions mprefix = defaultOptions { omitNothingFields = True, fieldLabelModifier = unPrefix . unLens }
  where
    unLens :: String -> String
    unLens ('_':s) = s
    unLens s = s
    unPrefix :: String -> String
    unPrefix = maybe id (\pre s -> fromMaybe s $ stripPrefix pre s) mprefix

cytoscapeCDN :: Monad m => HtmlT m ()
cytoscapeCDN = script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.1.2/cytoscape.min.js"] ""

data EdgeData = EdgeData {
    _edgedata_id     :: Text
  , _edgedata_source :: Text
  , _edgedata_target :: Text
} deriving (Show, Generic)

makeLenses ''EdgeData

instance ToJSON EdgeData where
  toJSON = genericToJSON $ jsonOptions (Just "edgedata_")

data Edge = Edge {
  _edge_data :: EdgeData
} deriving (Show, Generic)

makeLenses ''Edge

instance ToJSON Edge where
  toJSON = genericToJSON $ jsonOptions (Just "edge_")

data NodeData = NodeData {
    _nodedata_id   :: Text
  , _nodedata_name :: Text
} deriving (Show, Generic)

makeLenses ''NodeData

instance ToJSON NodeData where
  toJSON = genericToJSON $ jsonOptions (Just "nodedata_")

data Node = Node {
    _node_data :: NodeData
} deriving (Show, Generic)

makeLenses ''Node

instance ToJSON Node where
  toJSON = genericToJSON $ jsonOptions (Just "node_")

data Layout = Layout {
    _layout_name :: Text
} deriving (Show, Generic)

makeLenses ''Layout

instance ToJSON Layout where
  toJSON = genericToJSON $ jsonOptions (Just "layout_")

data Style = Style {
  _style_selector :: Text,
  _style_style :: [(Text,Text)]
} deriving (Show, Generic)

makeLenses ''Style

instance ToJSON Style where
  toJSON = genericToJSON $ jsonOptions (Just "style_")

runCytoscape :: [Node] -> [Edge] -> Layout -> [Style] -> Text -> Text -> Text
runCytoscape nodes edges layout styles morejs element = T.unlines [
  "",
  "$(function(){",
  "var cy = cytoscape({",
  "  container: document.querySelector('"<>element<>"'),",
  "  elements: {",
  "    nodes: " <> (decodeUtf8 $ toStrict $ encode nodes)<>",",
  "    edges: " <> (decodeUtf8 $ toStrict $ encode edges),
  "  },",
  "  layout: " <> (decodeUtf8 $ toStrict $ encode layout)<>",",
  "  style: " <> (decodeUtf8 $ toStrict $ encode styles),
  "});",
  morejs,
  "})"
  ]
