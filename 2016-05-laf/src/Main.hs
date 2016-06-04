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
  mot1,
  mot2,
  mot3,
  mot4,
  slide "Deactivating Rules" "" $ const [],
  slide "Enforcing Arguments" "" $ const [],
  slide "Conditional Acceptability" "" $ const [],
  slide "Conclusion" "" $ const []
  ] where
    dt = slideShowData &
      title .~ "Accepting a Decision with Aspic+" &
      author .~ "Jann Müller" &
      date .~ "19 May 2016"
      
mot1 = slide "Motivation" "Decision Making" $ const [
    grid & children .~ [
        columnTen & children .~ [ bullets [
            "Form arguments pro and con each option from a knowledge base",
            "Options in a decision correspond to preferred extensions",
            "$\\Sigma_{\\mathsf{pr}}(G)$ "]],
        columnSix -- & children .~ [div & attributes . at "class" ?~ "cy" & callbacks . elementCreated ?~ renderSimpleGraph]
    ]]

mot2 = slide "Motivation" "Decision Making" $ const [
    grid & children .~ [
        columnTen & children .~ [ bullets [
            -- p & content .~ "Example",
            "$KB = \\{ r_1, r_2, r_3, \\Rightarrow \\mathtt{a} \\}$",
            "$r_1 = \\mathtt{a} \\Rightarrow \\mathtt{b}, r_2 = \\mathtt{a} \\Rightarrow \\neg \\mathtt{b}, r_3 = \\mathtt{b} \\Rightarrow \\mathtt{d} $",
            "Arguments",
            "$A = [ \\Rightarrow \\mathtt{a}]$",
            "$B = [A; r_1; \\mathtt{b}]$",
            "$C = [A; r_2; \\neg \\mathtt{b}]$",
            "$D = [B; r_3; \\mathtt{d}]$"]],
        columnSix & children .~ [ div & attributes . at "class" ?~ "cy" & callbacks . elementCreated ?~ renderExample1 ]
        ]
    ]

example1 :: [GraphElement] 
example1 = [
    Node $ nodeData & nodeId .~ "A" & isSelected .~ True,
    Node $ nodeData & nodeId .~ "B" & isSelected .~ True,
    Node $ nodeData & nodeId .~ "C",
    Node $ nodeData & nodeId .~ "D" & isSelected .~ True,
    Edge $ edgeData "B" "C",
    Edge $ edgeData "C" "B",
    Edge $ edgeData "C" "D"]
    
renderExample1 :: T.Text -> IO ()
renderExample1 target = renderCytoscape target example1 DefaultGraphStyle CoseBilkent

mot3 = slide "Motivation" "Decision Acceptance" $ const [bullets [  
    "How to make $E = \\{ A, B, D \\}$ sceptically acceptable?",
    "Need to find a knowledge base $KB'$ such that",
    "$KB \\subseteq KB'$", 
    "$E \\subseteq \\Sigma_{\\mathsf{gr}}(\\mathsf{argGraph}(KB'))$" ]]
    
mot4 = slide "Motivation" "Deactivating Rules" $ const [bullets [
    "$r_2 = \\mathtt{a} \\Rightarrow \\neg \\mathtt{b}$ is causing the problem",
    "So we add a new rule $\\neg \\langle r_2 \\rangle$"
    ]]
    
notation = slide "Motivation" "Notation" $ const [bullets [
    "$KB$: Set of defeasible rules in Aspic+",
    "$\\langle \\r \\rangle$: Name of defeasible rule $r$"
    ]]