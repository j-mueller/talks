{-# LANGUAGE OverloadedStrings #-}
-- | Constructors for SemanticUI elements
-- cf. http://semantic-ui.com/
--  
module SemanticUI(
    menu,
    container,
    headerItem,
    item,
    rightMenu
) where
  
import Control.Lens hiding (children, transform)

import VirtualHom.Element
import VirtualHom.Html hiding (content, main, menu, footer)
import VirtualHom.Rendering(renderingOptions, onElementChanged)

import Prelude hiding (div)

menu = div & attributes . at "class" ?~ "ui fixed menu"

rightMenu = div & attributes . at "class" ?~ "right menu"  

headerItem = div & attributes . at "class" ?~ "header item"

item = div & attributes . at "class" ?~ "item"

container = div & attributes . at "class" ?~ "ui container"