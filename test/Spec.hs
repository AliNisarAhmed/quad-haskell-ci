module Main where

import Core
import qualified Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.ByteString as ByteString
import qualified Runner
import qualified System.Process.Typed as Process
import Test.Hspec

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
    it "should share workspace between steps" do
      testSharedWorkspace docker runner
    it "should collect logs" do
      testLogCollection runner

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"
  Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ -> pure newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild

-- Helper Functions

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Docker.Image image,
      commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline {steps = NonEmpty.Partial.fromList steps}

-- first test

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "First Step" "ubuntu" ["date"],
          makeStep "Second Step" "ubuntu" ["uname -r"]
        ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [makeStep "Should Fail" "ubuntu" ["exit 1"]]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "Create file" "ubuntu" ["echo hello > test"],
          makeStep "Read file" "ubuntu" ["cat test"]
        ]

  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]
  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          case ByteString.breakSubstring word log.output of
            (_, "") -> pure () -- Not Found
            _ -> modifyMVar_ expected (pure . Set.delete word)
  let hooks = Runner.Hooks { logCollected = onLog }

  build <- runner.prepareBuild $ makePipeline
                                  [ makeStep "Long Step" "ubuntu" ["echo hello", "sleep 2", "echo world"]
                                  , makeStep "Echo Linux" "ubuntu" ["uname -s"]
                                  ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  readMVar expected >>= \logs -> logs `shouldBe` Set.empty


--

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks {logCollected = \_ -> pure ()}
