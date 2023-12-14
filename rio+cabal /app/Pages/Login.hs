{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Pages.Login (loginPage, LoginPage) where 

import RIO
import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
import Servant ((:>))
import qualified Servant as S
import Servant.HTML.Blaze(HTML)

loginPageHtml :: Html
loginPageHtml =  H.docTypeHtml $ do
  H.head $ do
            H.title "LoginPage"
  H.body $ do
            H.h1 "Its Login Time!"
            H.p "lorem ipsum"



loginPage :: S.ServerT LoginPage (RIO env) 
loginPage = pure loginPageHtml 

type LoginPage = "login" :> S.Get '[HTML] Html