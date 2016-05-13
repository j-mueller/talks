{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Simple model for slides
module Slides where
  
import Control.Lens hiding (children)
import Data.Foldable (foldl')
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import VirtualHom.Element
import VirtualHom.View(View, renderUI, specialise)
import VirtualHom.Html (h1, h2, h3, h4, p, div)
import VirtualHom.Rendering(renderingOptions, onElementChanged)
import Prelude hiding (div)
  
import SemanticUI
import MathJax

-- | State = View that can be navigated to
-- | Slide = pointer to a specific view

data Slide = Slide { 
  _section :: T.Text, 
  _subsection :: T.Text, 
  _contents :: View Identity () }
  
makeLenses ''Slide

-- | Create a slide
slide :: T.Text -> T.Text -> View Identity () -> Slide
slide = Slide

data SlideShowData = SlideShowData {
  _title :: T.Text,
  _author :: T.Text,
  _date :: T.Text,
  _subtitle :: T.Text
}

makeLenses ''SlideShowData

slideShowData :: SlideShowData
slideShowData = SlideShowData "" "" "" ""

data SlideShow = SlideShow {
  _metadata :: SlideShowData,
  _current :: Slide,
  _previous :: Maybe SlideShow,
  _next :: Maybe SlideShow
}

makeLenses ''SlideShow
  
slideDeck :: SlideShowData -> [Slide] -> SlideShow
slideDeck dt slides = slideDeck' dt [] slides firstSlide where
  firstSlide = slide "" "" (const [div & children .~ [h1 $ dt^.title, h2 $ dt^.subtitle, h4 $ dt^.author, h4 $ dt^.date]])

slideDeck' :: SlideShowData -> [Slide] -> [Slide] -> Slide -> SlideShow
slideDeck' dt left right cur = SlideShow dt cur pr nxt where
  pr  = fmap (sd (tail left) (cur:right))  $ listToMaybe left
  nxt = fmap (sd (cur:left)  (tail right)) $ listToMaybe right
  sd = slideDeck' dt

-- | Render a slide show in a given div
renderSlideShow :: T.Text -> SlideShow -> IO ()
renderSlideShow target shw = do
  let options = onElementChanged updateMathJax $ renderingOptions target
  let interp = return . runIdentity
  renderUI options showSlide interp shw
  
showSlide :: View Identity SlideShow
showSlide ss = [container & 
  callbacks . keydown ?~ onKeyDown &
  children .~ [
    menu & children .~ [
      container & children .~ [
        headerItem & content .~ ss^.metadata.title
      ]
    ],
    div & attributes . at "class" ?~ "main-div" & children .~ [
      div & attributes . at "class" ?~ "main-div" & children .~[
        h2 $ ss^.current.section,
        h3 $ ss^.current.subsection,
        div & children .~ (specialise united (ss^.current.contents) ss)
      ]]
    ] ++ (maybe [] leftBtn $ ss^.previous) ++ (maybe [] rightBtn $ ss^.next)
  ] where
      moveLeft s  = return $ maybe s id (s^.previous)
      moveRight s = return $ maybe s id (s^.next)
      leftBtn sls = [button & 
        content .~ "<" &
        callbacks . click ?~ (const moveLeft)]
      rightBtn sls = [button & 
        content .~ ">" &
        callbacks . click ?~ (const moveRight)]
      onKeyDown args = moveRight
    
