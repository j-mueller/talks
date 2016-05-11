{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding (children, transform)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import qualified Data.Text as T
import Prelude hiding (div)

import VirtualHom.Element
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions, onElementChanged)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.View(View, renderUI)

import Slides
import MathJax

theUI :: View Identity ()
theUI () = [container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p & content .~ "I am a paragraph!",
      p & content .~ "With math",
      p & content .~ "$(a \\wedge b) \\Rightarrow (a \\vee b)$"]]]

main :: IO ()
main = do
  let options = onElementChanged updateMathJax $ renderingOptions "virtual-hom"
  let interp = return . runIdentity
  renderUI options theUI interp ()