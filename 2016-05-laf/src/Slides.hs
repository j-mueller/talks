{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Simple model for slides
module Slides where
  
import Control.Lens hiding (children)
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
  _next :: Maybe SlideShow,
  _groups :: [(T.Text, SlideShow)]
}

makeLenses ''SlideShow
  
slideDeck :: SlideShowData -> [Slide] -> SlideShow
slideDeck dt slides = result where
  firstSlide = slide "" "" (const [div & children .~ [h1 $ dt^.title, h2 $ dt^.subtitle, h4 $ dt^.author, h4 $ dt^.date]])
  result = SlideShow dt firstSlide Nothing Nothing []

-- | Render a slide show in a given div
renderSlideShow :: T.Text -> SlideShow -> IO ()
renderSlideShow target shw = do
  let options = onElementChanged updateMathJax $ renderingOptions target
  let interp = return . runIdentity
  renderUI options showSlide interp shw
  
showSlide :: View Identity SlideShow
showSlide ss = [container & children .~ [
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
    ]]
