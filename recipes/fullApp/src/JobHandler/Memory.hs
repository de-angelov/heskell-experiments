module JobHandler.Memory where 

import RIO 
import Core 

import qualified JobHandler.JobHandler  as JobHandler
import qualified RIO.Map as Map
import qualified Control.Concurrent.STM as STM 
import qualified Agent
import qualified RIO.List as List 
import Core (Build(pipeline))

dispatchCmd_  :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ state = 
    case List.find queued $ Map.toList state.jobs of 
        Just (number, job) -> 
            let updatedJob = job{state = JobHandler.JobAssigned}
                updatedState = Map.insert number updatedJob state.jobs 
                cmd = Just $ Agent.StartBuild number job.pipeline 
            in (cmd, state {jobs = updatedState} )

        _ -> (Nothing, state)
    where queued (_, job) = job.state == JobHandler.JobQueued 

data State
    = State 
    { jobs :: Map Agent.BuildNumber JobHandler.Job 
    , logs :: Map (Agent.BuildNumber, StepName) ByteString
    , nextBuild :: Int 
    } deriving (Eq, Show)


findJob_ :: Agent.BuildNumber -> State -> Maybe JobHandler.Job 
findJob_ number state = 
    Map.lookup number state.jobs 

processMsg_ :: Agent.Msg -> State -> State 
processMsg_ msg state = case msg of 
    Agent.BuildUpdated number build -> 
        let f job = job{state = JobHandler.JobScheduled build}
        in state{jobs = Map.adjust f number state.jobs}
    
    Agent.LogCollected number log -> 
        let updatedLogs = Map.insertWith (flip mappend) (number, log.step) log.output state.logs 
        in  state{logs = updatedLogs}

fetchLogs_ :: Agent.BuildNumber -> StepName -> State -> Maybe ByteString 
fetchLogs_ number step state = 
    Map.lookup (number, step) state.logs

createService :: IO JobHandler.Service 
createService = do 
    
    state <- STM.newTVarIO State 
        { jobs = mempty 
        , logs = mempty 
        , nextBuild = 1 
        }


    pure JobHandler.Service
        { queueJob = \info pipeline -> STM.atomically do 
            STM.stateTVar state $ queueJob_ info pipeline
        , fetchLogs = \number step -> STM.atomically do 
            s <- STM.readTVar state 
            pure $ fetchLogs_ number step s 
        , latestJobs = STM.atomically do 
            s <- STM.readTVar state 
            pure $ latestJobs_ s 
        , findJob = \number -> STM.atomically do 
            s <- STM.readTVar state 
            pure $ findJob_ number s
        , dispatchCmd = STM.atomically do 
            STM.stateTVar state dispatchCmd_ 
        , processMsg = \msg -> STM.atomically do 
            STM.modifyTVar' state $ processMsg_ msg 
        }

latestJobs_ :: State -> [(Agent.BuildNumber, JobHandler.Job)]
latestJobs_  state = List.reverse $ Map.toList state.jobs 

queueJob_ :: JobHandler.CommitInfo -> Pipeline -> State -> (Agent.BuildNumber, State)
queueJob_ info pipeline state = 
    (number, updatedState)
    where 
        number = Agent.BuildNumber state.nextBuild
        job = JobHandler.Job 
            { pipeline = pipeline 
            , state = JobHandler.JobQueued 
            , info = info

            }
        
        updatedState = 
            state 
                { jobs = Map.insert number job state.jobs
                , nextBuild = state.nextBuild + 1 
                }
