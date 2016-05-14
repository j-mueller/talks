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
import SemanticUI
import Cytoscape

main :: IO ()
main = renderSlideShow "virtual-hom" myTalk

myTalk :: SlideShow
myTalk = slideDeck dt [
  motivation1,
  motivation2,
  slide "Deactivating Rules" "" $ const [],
  slide "Enforcing Arguments" "" $ const [],
  slide "Conclusion" "" $ const []
  ] where
    dt = slideShowData &
      title .~ "Accepting a Decision with Aspic+" &
      author .~ "Jann Müller" &
      date .~ "19 May 2016"
      
motivation1 = slide "Motivation" "Decision Making" $ const [
    grid & children .~ [
        columnTen & children .~ [ bullets [
            "Form arguments pro and con each option from a knowledge base",
            "Options in a decision correspond to preferred extensions",
            "$\\Sigma_{\\mathsf{pr}}(G)$ "]],
        columnSix & children .~ [div & attributes . at "class" ?~ "cy" & callbacks . elementCreated ?~ renderSimpleGraph]]]

simpleGraph :: [GraphElement] 
simpleGraph = [
    Node $ nodeData & nodeId .~ "a1",
    Node $ nodeData & nodeId .~ "a2",
    Node $ nodeData & nodeId .~ "a3",
    Edge $ edgeData "a1" "a2",
    Edge $ edgeData "a2" "a1",
    Edge $ edgeData "a3" "a2"]
    
renderSimpleGraph :: T.Text -> IO ()
renderSimpleGraph target = renderCytoscape target simpleGraph DefaultGraphStyle CoseBilkent

motivation2 = slide "Motivation" "Decision Acceptance" $ const [bullets [  
    "Once an option has been chosen we want to elevate its arguments to be sceptically acceptable"]]
    
