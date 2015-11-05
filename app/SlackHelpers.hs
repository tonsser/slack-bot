module SlackHelpers
    ( textWithoutTriggerWord
    , matchingAction
    , processRequest
    )
  where

import Import
import SlackTypes
import qualified Data.List.Utils as L
import qualified Data.Text as T
import qualified Text.Regex as R
import System.Environment
import qualified BotAction as BA
import BotAction (BotAction)
import qualified Network.HTTP.Conduit as HTTP
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

slackUrl :: String
slackUrl = "https://hooks.slack.com/services/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"

postResponseToSlack :: SlackResponseDestination -> Text -> IO ()
postResponseToSlack destination text = do
    initReq <- parseUrl slackUrl
    let
      res = SlackResponse text destination

      body :: LBS.ByteString
      body = encode res

      httpReq :: HTTP.Request
      httpReq = initReq { method = "POST"
                        , requestBody = RequestBodyLBS body
                        }
    man <- HTTP.newManager HTTP.tlsManagerSettings
    void $ HTTP.httpLbs httpReq man

authenticateAction :: Text -> Text -> BotAction
authenticateAction username teamId postToSlack _ = do
    appRoot <- lookupEnv "APPROOT"
    case appRoot of
      Nothing -> postToSlack "Error getting APPROOT environment variable"
      Just appRoot' -> do
        let authUrl = appRoot' ++ "&team_id = " ++ T.unpack teamId
        postResponseToSlack (SlackResponseUsername username) $ T.pack authUrl
        _ <- postToSlack "Check your private messages"
        return $ Right ()

processRequest :: SlackRequest -> IO ()
processRequest req =
    let
      text :: String
      text = T.unpack $ textWithoutTriggerWord req

      match :: Maybe ([String], BotAction)
      match = matchingAction text $ authAction : BA.actions
        where username = slackRequestUsername req
              teamId = slackRequestTeamId req
              authAction = ("authenticate", authenticateAction username teamId)

      destination :: SlackResponseDestination
      destination = SlackResponseChannel $ slackRequestChannelName req

      postString txt = do
        postResponseToSlack destination $ T.pack txt
        return $ Right ()
    in case match of
         Nothing -> void $ postString "Unknown action"
         Just (matches, action) -> do
           res <- action postString matches
           case res of
             Right () -> return ()
             Left reason -> void $ postString reason

matchingAction :: String -> [(String, BotAction)] -> Maybe ([String], BotAction)
matchingAction t as = helper t $ map (`mapFst` R.mkRegex) as
  where
    helper :: String -> [(R.Regex, BotAction)] -> Maybe ([String], BotAction)
    helper _ [] = Nothing
    helper text ((regex, h) : rest) =
        let
          match :: Maybe [String]
          match = R.matchRegex regex text
        in case match of
             Nothing -> helper text rest
             Just matches -> Just (matches, h)

textWithoutTriggerWord :: SlackRequest -> Text
textWithoutTriggerWord req = T.strip $ T.pack $ L.replace (triggerWord req) "" $ T.unpack $ slackRequestText req
  where
    triggerWord :: SlackRequest -> String
    triggerWord = T.unpack . slackRequestTriggerWord

mapFst :: (a, b) -> (a -> c) -> (c, b)
mapFst (a, b) f = (f a, b)
