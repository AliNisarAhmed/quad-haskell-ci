module Runner where

import Core
import qualified Docker
import RIO

data Service = Service
  { runBuild :: Hooks -> Build -> IO Build,
    prepareBuild :: Pipeline -> IO Build
  }

createService :: Docker.Service -> IO Service
createService docker = do
  pure Service {runBuild = runBuild_ docker, prepareBuild = prepareBuild_ docker}

runBuild_ :: Docker.Service -> Hooks -> Build -> IO Build
runBuild_ docker hooks build =
  loop build $ Core.initLogCollection build.pipeline
  where
    loop :: Build -> LogCollection -> IO Build
    loop build collection = do
      (newCollection, logs) <- Core.collectLogs docker collection build
      newBuild <- Core.progress docker build
      hooks.buildUpdated newBuild
      traverse_ hooks.logCollected logs
      case newBuild.state of
        BuildFinished _ ->
          pure newBuild
        _ -> do
          threadDelay (1 * 1000 * 1000)
          loop newBuild newCollection


-- sets up a volume for a build and initiates a build
prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  volume <- docker.createVolume
  pure Build {pipeline = pipeline, state = BuildReady, completedSteps = mempty, volume = volume}

---

data Hooks = Hooks {logCollected :: Log -> IO (), buildUpdated :: Build -> IO ()}
