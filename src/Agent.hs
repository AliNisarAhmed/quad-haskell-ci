module Agent where

import RIO
import Core

import qualified Codec.Serialise as Serialise

data Cmd
    = StartBuild BuildNumber Pipeline
    deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
    = LogCollected BuildNumber Log
    | BuildUpdated BuildNumber Build
    deriving (Eq, Show, Generic, Serialise.Serialise)

