module LocalRequests
    ( LocalRequest(..)
    , runResponsesLocally
    , makeLocalRequest
    )
  where

import Import
import SlackTypes
import BotResponder
import qualified BotAction as B

runResponsesLocally :: BotRequest r => r -> IO [String]
runResponsesLocally req = do
  responses <- responsesForRequest Nothing req
  case responses of
    Right rs -> do
      var <- newIORef []
      gatherResponses rs var
    Left err -> return $ [err]

data LocalRequest = LocalRequest
                  { localRequestToken       :: Text
                  , localRequestTeamId      :: Text
                  , localRequestTeamDomain  :: Text
                  , localRequestChannelId   :: Text
                  , localRequestChannelName :: Text
                  , localRequestTimestamp   :: Text
                  , localRequestUserId      :: Text
                  , localRequestUsername    :: Text
                  , localRequestText        :: Text
                  , localRequestTriggerWord :: Text
                  }

makeLocalRequest :: LocalRequest
makeLocalRequest = LocalRequest
                 { localRequestToken       = "xxx"
                 , localRequestTeamId      = "T0001"
                 , localRequestTeamDomain  = "example"
                 , localRequestChannelId   = "C2147483705"
                 , localRequestChannelName = "test"
                 , localRequestTimestamp   = "123"
                 , localRequestUserId      = "U2147483697"
                 , localRequestUsername    = "hest"
                 , localRequestText        = "bot help"
                 , localRequestTriggerWord = "bot"
                 }

instance BotRequest LocalRequest where
  requestChannelId   = localRequestChannelId
  requestChannelName = localRequestChannelName
  requestTeamDomain  = localRequestTeamDomain
  requestTeamId      = localRequestTeamId
  requestText        = localRequestText
  requestTimestamp   = localRequestTimestamp
  requestToken       = localRequestToken
  requestTriggerWord = localRequestTriggerWord
  requestUserId      = localRequestUserId
  requestUsername    = localRequestUsername

gatherResponses :: [B.ResponseRunner] -> IORef [String] -> IO [String]
gatherResponses [] var = readIORef var
gatherResponses (resp : rest) var = do
    _ <- resp $ \x -> do contents <- readIORef var
                         writeIORef var (x : contents)
                         return x
    gatherResponses rest var

