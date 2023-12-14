{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Pages.Secret (secretPageHtml, SecretPage) where 
import Servant ((:>))
import qualified Servant as S
import Servant.HTML.Blaze(HTML)

secretPageHtml :: Html
secretPageHtmlPageHtml =  H.docTypeHtml $ do
  H.head $ do
            H.title "Secret Page"
  H.body $ do
            H.h1 "TOP Secret!"
            H.p "lorem ipsum"
            H.p "lorem ipsum"

secretPage = pure secretPageHtml 

type SecretPage = "secret" :> S.Get '[HTML] Html