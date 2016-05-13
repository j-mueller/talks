{-# LANGUAGE OverloadedStrings #-}
-- | Constructors for SemanticUI elements
-- cf. http://semantic-ui.com/
--  
module SemanticUI(
    buttons,
    button,
    menu,
    container,
    headerItem,
    item,
    rightMenu
) where
  
import Control.Lens hiding (children, transform)

import VirtualHom.Element
import VirtualHom.Html hiding (button, content, main, menu, footer)
import qualified VirtualHom.Html as H
import VirtualHom.Rendering(renderingOptions, onElementChanged)

import Prelude hiding (div)

menu = div & attributes . at "class" ?~ "ui fixed menu"

rightMenu = div & attributes . at "class" ?~ "right menu"  

headerItem = div & attributes . at "class" ?~ "header item"

item = div & attributes . at "class" ?~ "item"

container = div & attributes . at "class" ?~ "ui container"

buttons = div & attributes . at "class" ?~ "ui buttons"

button = H.button & attributes . at "class" ?~ "ui button"