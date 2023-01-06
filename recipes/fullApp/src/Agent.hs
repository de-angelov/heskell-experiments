module Agent where

import RIO
import Core
import qualified Codec.Serialise as Serialise
import qualified Network.HTTP.Simple as HTTP
import qualified Runner
import qualified System.Log.Logger as Logger

data Cmd
    = StartBuild BuildNumber Pipeline
    deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
    = LogCollected BuildNumber Log
    | BuildUpdated BuildNumber Build
    deriving (Eq, Show, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int
    deriving (Eq, Ord, Show, Generic, Serialise.Serialise)

data Config
    = Config
    { endpoint :: String
    }


sendMessage :: Config -> Msg -> IO ()
sendMessage config msg = do
    base <- HTTP.parseRequest config.endpoint

    let body = Serialise.serialise msg
    let req = base
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestPath "/agent/send"
            & HTTP.setRequestBodyLBS body

    void $ HTTP.httpBS req



buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

runCommand :: Config -> Runner.Service -> Cmd -> IO ()
runCommand config runner  = \case
    StartBuild number pipeline -> do
        let hooks = Runner.Hooks
                    { logCollected = \log -> do
                        sendMessage config $ LogCollected number log
                    , buildUpdated = \build -> do
                        sendMessage config $ BuildUpdated number build
                    }

        let n = Core.displayBuildNumber number 
        Logger.infoM "quad.agent" $ "Starting build" <> n

        build <- (runner :: Runner.Service).prepareBuild pipeline
        void $ (runner :: Runner.Service).runBuild hooks build

        Logger.infoM "quad.agent" $ "Finished build" <> n

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
    endpoint <- HTTP.parseRequest config.endpoint

    let req = endpoint
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestPath "/agent/pull"

    res <- HTTP.httpLBS req


    let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd

    traverse_ (runCommand config runner) cmd
        `catch` \e -> do
        Logger.warningM "quad.agent" "Server offline,  waiting..."
        Logger.warningM "quad.agent" $ show (e :: HTTP.HttpException)
        pure ()



    threadDelay (1 * 1000 * 1000)