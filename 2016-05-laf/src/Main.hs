{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding (children, transform)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import qualified Data.Text as T
import Prelude hiding (div)

import VirtualHom.Element
import VirtualHom.Html (h1, p, div)
import VirtualHom.Rendering(renderingOptions, onElementChanged)
import VirtualHom.View(View, renderUI)

import Slides

main :: IO ()
main = renderSlideShow "virtual-hom" myTalk

myTalk :: SlideShow
myTalk = slideDeck dt [
  slide "Motivation" "" $ const [],
  slide "Deactivating Rules" "" $ const [],
  slide "Enforcing Arguments" "" $ const [],
  slide "Conclusion" "" $ const []
  ] where
    dt = slideShowData &
      title .~ "Accepting a Decision with Aspic+" &
      author .~ "Jann Müller" &
      date .~ "19 May 2016"
      
