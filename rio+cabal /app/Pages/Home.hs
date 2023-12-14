{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Pages.Home (homePage, HomePage) where 

import RIO
import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
import Servant.HTML.Blaze(HTML)
import Servant ((:>) )
import qualified Servant as S
import Types (AppConfig)

homePageHtml :: Html
homePageHtml =  H.docTypeHtml $ do
  H.head $ do
            H.title "HomePage"
  H.body $ do
            H.h1 "Home Sweet Home!"
            H.p "lorem ipsum"
            H.p "lorem ipsum"

homePage :: S.ServerT HomePage (RIO env) 
homePage = pure homePageHtml 


type HomePage = "" :> S.Get '[HTML] Html