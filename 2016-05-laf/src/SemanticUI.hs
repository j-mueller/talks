{-# LANGUAGE OverloadedStrings #-}
-- | Constructors for SemanticUI elements
-- cf. http://semantic-ui.com/
--  
module SemanticUI(
    bullets,
    buttons,
    button,
    columnFour,
    columnEight,
    columnSix,
    columnTen,
    columnTwelve,
    disabledButton,
    grid,
    list,
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

disabledButton = H.button & attributes . at "class" ?~ "ui disabled button" 

list theChildren = ul 
    & attributes . at "class" ?~ "ui list"
    & children .~ (fmap (\c -> li & children .~ [c])) theChildren 
    
bullets = list . fmap (\b -> p & content .~ b)

grid = div & attributes . at "class" ?~ "ui grid"

columnFour = div & attributes . at "class" ?~ "four wide column" 

columnSix = div & attributes . at "class" ?~ "six wide column" 

columnEight = div & attributes . at "class" ?~ "eight wide column"

columnTen = div & attributes . at "class" ?~ "ten wide column"

columnTwelve = div & attributes . at "class" ?~ "twelve wide column"