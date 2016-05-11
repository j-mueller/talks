{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE TemplateHaskell#-}
-- | Run MathJax on VirtualHom elements when they are created
module MathJax(updateMathJax) where

import Data.JSString.Text (textToJSString)
import GHCJS.Types (JSString)
  
import qualified Data.Text as T

-- | Run mathjax on a DOM element identified by its ID
updateMathJax :: T.Text -> IO ()
updateMathJax i = case i of
  "" -> return ()
  _  -> js_mathJaxQueue $ textToJSString i
  
foreign import javascript unsafe "MathJax.Hub.Queue(['Typeset',MathJax.Hub,$1]);"
  js_mathJaxQueue :: JSString -> IO ()
