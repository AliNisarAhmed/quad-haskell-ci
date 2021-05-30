module Server where

import RIO
import Core

import qualified JobHandler

import qualified Web.Scotty as Scotty

data Config
  = Config
      { port :: Int
      }

run :: Config -> JobHandler.Service -> IO ()
run config handler =
    Scotty.scotty config.port do
      pure ()