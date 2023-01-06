module GitHub where 

import RIO 
import Core 
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson 
import qualified Data.Aeson.Types as Aeson.Types 
import qualified RIO.Text as Text 

import qualified JobHandler.JobHandler as JobHandler 

import qualified Data.Yaml as Yaml 
import qualified Network.HTTP.Simple as HTTP 
import qualified Docker 
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial 

createCloneStep :: JobHandler.CommitInfo -> Step 
createCloneStep info = Step 
    { name = StepName "clone"
    , commands = NonEmpty.Partial.fromList 
        [ "git clone -q https://github.com" <> info.repo <> " ."
        , "git checkout -qf " <> info.sha
        ]
    , image = Docker.Image "alpine.git" "v2.26.2"
    }

fetchRemotePipeline :: JobHandler.CommitInfo -> IO  Pipeline 
fetchRemotePipeline info = do 
    endpoint <- HTTP.parseRequest "https://api.github.com"
    let path = "/path/" <> info.repo <> "/contents/.quad.yml"

    let req = endpoint 
            & HTTP.setRequestPath (encodeUtf8 path)
            & HTTP.addToRequestQueryString [("ref", Just $ encodeUtf8 info.sha)]
            & HTTP.addRequestHeader "User-Agent" "quad-ci"
            & HTTP.addRequestHeader "Accept" "application/vnd.github.v3.raw"

    res <- HTTP.httpBS req 

    Yaml.decodeThrow $ HTTP.getResponseBody res

parsePushEvent :: ByteString -> IO JobHandler.CommitInfo 
parsePushEvent body = do 
    let parser = Aeson.withObject "github-webhook" $ \event -> do 
            branch <- event .: "ref" <&> \ref -> 
                Text.dropPrefix "refs/heads/" ref 

            
            commit <- event .: "head_commit"
            sha <- commit .: "id"
            message <- commit .: "message"
            author <- commit .: "author" >>= \a -> a .: "username"
            
            repo <- event .: "repository" >>= \r -> r .: "full_name"

            pure JobHandler.CommitInfo 
                    { sha = sha 
                    , repo = repo 
                    , branch = branch 
                    , message = message 
                    , author = author 
                    }
    let result = do 
            value <- Aeson.eitherDecodeStrict body 
            Aeson.Types.parseEither parser value 

    case result of 
        Left e -> throwString e 
        Right info -> pure info



