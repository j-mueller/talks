{-# LANGUAGE OverloadedStrings #-}
-- | Simple model for slides
module Slides where
  
import qualified Data.Text as T
import VirtualHom.Element
import VirtualHom.View(View)
  
-- | State = View that can be navigated to
-- | Slide = pointer to a specific view

data Slide m t = Slide { _title :: T.Text, views :: [View m t] }

-- | Create a slide
slide :: T.Text -> [View m t] -> Slide m t
slide = Slide

myTalk :: [Slide m ()] 
myTalk = [slide "Accepting a Decision with Aspic+" []]