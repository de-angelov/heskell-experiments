module JobHandler.JobHandler where 

import Agent 
import RIO 
import Core 
import qualified Data.Aeson as Aeson
import qualified Codec.Serialise as Serialise

data Job 
    = Job 
    { pipeline :: Pipeline 
    , state :: JobState
    , info :: CommitInfo 
    } deriving (Eq, Show)

data JobState
    = JobQueued 
    | JobAssigned 
    | JobScheduled Build 
    deriving (Eq, Show)

data CommitInfo 
    = CommitInfo 
        { sha :: Text
        , branch :: Text 
        , message :: Text 
        , author :: Text 
        , repo :: Text 
        } deriving (Eq, Show, Generic, Aeson.ToJSON,  Aeson.FromJSON, Serialise.Serialise)

data Service 
    = Service 
    { queueJob :: CommitInfo -> Pipeline -> IO BuildNumber 
    , dispatchCmd :: IO (Maybe Agent.Cmd)
    , processMsg :: Agent.Msg -> IO ()
    , findJob :: BuildNumber -> IO (Maybe Job)
    , fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString)
    , latestJobs :: IO [(BuildNumber, Job)]
    }

