module JobHandler where

import qualified Agent
import Core
import qualified Data.Aeson as Aeson
import RIO

data Job = Job
  { pipeline :: Pipeline,
    state :: JobState,
    info :: CommitInfo
  }
  deriving (Eq, Show)

data JobState
  = JobQueued
  | JobAssigned
  | JobScheduled Build
  deriving (Eq, Show)

data CommitInfo = CommitInfo
  { sha :: Text,
    repo :: Text,
    branch :: Text,
    message :: Text,
    author :: Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data Service = Service
  { queueJob :: CommitInfo -> Pipeline -> IO BuildNumber, -- once a job is queued, it's an Agent's job to poll the server to ask for work
    dispatchCmd :: IO (Maybe Agent.Cmd), -- checks the store and see if there is any command to be given to the polling Agent
    processMsg :: Agent.Msg -> IO (),
    findJob :: BuildNumber -> IO (Maybe Job),
    fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString),
    latestJobs :: IO [(BuildNumber, Job)]
  }
