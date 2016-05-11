{-# LANGUAGE OverloadedStrings #-}
-- | Constructors for SemanticUI elements
-- cf. http://semantic-ui.com/
--  
module SemanticUI(
    menu,
    container,
    headerItem
) where
  
import Control.Lens hiding (children, transform)

import VirtualHom.Element
import VirtualHom.Html hiding (content, main, menu)
import VirtualHom.Rendering(renderingOptions, onElementChanged)

import Prelude hiding (div)

menu = div & attributes . at "class" ?~ "ui fixed menu" 

headerItem = div & attributes . at "class" ?~ "header item"

container = div & attributes . at "class" ?~ "ui container"