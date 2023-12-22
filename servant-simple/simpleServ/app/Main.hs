{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}


module Main  where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze(HTML)
import Text.Blaze.Html5 as H hiding (main)


type HtmlPage = "html" :> Get '[HTML] H.Html

htmlPageServer :: Server HtmlPage
htmlPageServer = pure $ H.docTypeHtml $ do
  H.head $ do 
    H.title "Test HTML PAGE"
  H.body $ do 
    H.h1 "Hello World !!!!"
    H.p "lorem ipsum"
    H.p "lorem ipsum"

type HomeAPI = Get '[PlainText] String


homeServer :: Server HomeAPI
homeServer = return "Hello"


type FactoringAPI = 
  "x" :> Capture "x" Int :>
  ( QueryParam "y" Int :> Get '[JSON] Int 
  :<|> Post '[JSON] Int 
  )

type API = 
  HomeAPI
  :<|> HtmlPage
  :<|> FactoringAPI 
  :<|> SimpleAPI "users" User UserId 
  :<|> SimpleAPI "products" Product ProductId 

factoringServer :: Server FactoringAPI 
factoringServer x = getXY :<|> postX
  where 
    getXY Nothing = return x 
    getXY (Just y) = return (x + y)

    postX = return (x - 1)

type SimpleAPI (name :: Symbol) a i = name :>
  ( Get '[JSON] [a]
  :<|> Capture "id" i :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

simpleServer 
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA = 
  listAs :<|> getA :<|> postA 

userServer :: Server (SimpleAPI "users" User UserId)
userServer = simpleServer 
  (return [])
  (\userId -> return $ 
    if userId == 0
    then User "john" 64 
    else User "everybody else" 10 
  ) 
  (\user -> return NoContent)

productServer :: Server (SimpleAPI "products" Product ProductId)
productServer = simpleServer 
  (return [])
  (\productId -> return $ Product "Great Stuff!")
  (\product -> return NoContent)

type UserId = Int 

data User = User { username :: String,  age :: Int }
  deriving Generic 

instance FromJSON User 
instance ToJSON User 

type ProductId = Int 

data Product = Product { productname :: String }
  deriving Generic 

instance FromJSON Product 
instance ToJSON Product 

api :: Proxy API
api = Proxy 

main :: IO ()
main = do
  putStrLn "Starting! server port 8080"

  run 8080 . serve api
    $ homeServer 
    :<|> htmlPageServer
    :<|> factoringServer 
    :<|> userServer 
    :<|> productServer
