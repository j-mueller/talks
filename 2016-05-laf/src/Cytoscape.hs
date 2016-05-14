{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TemplateHaskell#-}
-- | Graph layouts with cytoscape
-- cf. https://github.com/cytoscape/cytoscape.js
module Cytoscape where 
  
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T
import GHCJS.Marshal
import GHCJS.Types (JSString, JSVal, JSRef)

data NodeData = NodeData { 
    _nodeId :: T.Text, 
    _isSelected :: Bool,
    _isSelectable :: Bool,
    _isLocked :: Bool,
    _isGrabbable :: Bool,
    _classNames :: T.Text 
  } deriving (Eq, Ord, Show)
makeLenses ''NodeData

nodeData :: NodeData
nodeData = NodeData "" False True False True ""

instance ToJSON NodeData where
  toJSON nd = object [
      "group" .= ("nodes" :: String),
      "data" .= (object [ "id" .= (nd^.nodeId) ]),
      "selected" .= (nd^.isSelected),
      "selectable" .= (nd^.isSelectable),
      "locked" .= (nd^.isLocked),
      "grabbable" .= (nd^.isGrabbable),
      "classes" .= (nd^.classNames)
    ]

data EdgeData = EdgeData { _sourceId :: T.Text, _targetId :: T.Text } deriving (Eq, Ord, Show)
makeLenses ''EdgeData

edgeData :: T.Text -> T.Text -> EdgeData
edgeData = EdgeData

instance ToJSON EdgeData where
  toJSON ed = object [
    "data" .= object [
      "source" .= (ed^.sourceId),
      "target" .= (ed^.targetId)]
    ]

data GraphElement =  Node NodeData | Edge EdgeData
makePrisms ''GraphElement

instance ToJSON GraphElement where
  toJSON (Node nd) = toJSON nd
  toJSON (Edge ed) = toJSON ed

data GraphStyle = DefaultGraphStyle -- TODO: Allow config

instance ToJSON GraphStyle where
  toJSON DefaultGraphStyle = toJSON [
    object [
      "selector" .= ("node" :: String),
      "style" .= (object ["background-color" .= ("#ad1a66" :: String)])
      ], 
    object [
        "selector" .= ("edge" :: String),
        "style" .= (object [
          "target-arrow-shape" .= ("triangle" :: String),
          "width" .= (4 :: Int), 
          "line-color" .= ("#000" :: String),
          "target-arrow-color" .= ("#000" :: String)])
      ]
    ]

data Layout = CoseBilkent -- TODO: Additional layouts

instance ToJSON Layout where
  toJSON CoseBilkent = object ["name" .= ("cose-bilkent" :: String)]

renderCytoscape :: T.Text -> [GraphElement] -> GraphStyle -> Layout -> IO ()
renderCytoscape i elems style layout = do
  jsKey <- toJSVal_aeson i
  jsElems <- toJSVal_aeson elems
  jsStyle <- toJSVal_aeson style
  jsLayout <- toJSVal_aeson layout
  js_cytoscape jsKey jsElems jsStyle jsLayout

foreign import javascript unsafe "cytoscape({container:document.getElementById($1),elements:$2,style:$3,layout:$4})"
  js_cytoscape :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()
