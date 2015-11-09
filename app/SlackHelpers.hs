module SlackHelpers
    ( textWithoutTriggerWord
    , matchingAction
    , processRequest
    , getAppRoot
    , concatPaths
    , mapFst
    )
  where

import Import
import SlackTypes
import qualified Data.List.Utils as L
import qualified Data.Text as T
import qualified Text.Regex as R
import UrlHelpers
import qualified BotAction as BA
import BotAction (BotAction)
import qualified Network.HTTP.Conduit as HTTP
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

-- TODO: Change this to read from an ENV variable
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

authenticateAction :: SlackRequest -> BotAction
authenticateAction req = BA.UnauthticatedAction $ \postToSlack _ -> do
    -- TODO: Don't use environment variables here
    -- TODO: Concat the paths in a nicer way
    appRoot <- getAppRoot
    let
      (slackAuth, _) = mapFst (T.unpack . T.intercalate "/") (renderRoute SlackAuthR)
      params = [ ("team_id", slackRequestTeamId req)
               , ("user_id", slackRequestUserId req)
               ]
      username = slackRequestUsername req
      authUrl = appRoot ++ slackAuth ++ T.unpack (toQueryParams params)
    postResponseToSlack (SlackResponseUsername username) $ T.pack authUrl
    _ <- postToSlack "Check your private messages"
    return $ Right ()

processRequest :: Maybe Text -> SlackRequest -> IO ()
processRequest accessTokenM req =
    let
      text :: String
      text = T.unpack $ textWithoutTriggerWord req

      match :: Maybe ([String], BotAction)
      match = matchingAction text $ authAction : BA.actions
        where authAction = ("authenticate", authenticateAction req)

      destination :: SlackResponseDestination
      destination = SlackResponseChannel $ slackRequestChannelName req

      postString txt = do
        postResponseToSlack destination $ T.pack txt
        return $ Right ()
    in case match of
         Nothing -> void $ postString "Sorry but I don't understand that. Type \"bot help\" to get a list of things I understand."
         Just (matches, action) ->
           case action of
             BA.UnauthticatedAction action' -> do
               res <- action' postString matches
               case res of
                 Right () -> return ()
                 Left reason -> void $ postString reason

             BA.AuthenticatedAction action' ->
               case accessTokenM of
                 Nothing -> void $ postString "Authenticate required, type: \"bot authenticate\""
                 Just accessToken -> do
                   res <- action' accessToken postString matches
                   case res of
                     Right () -> return ()
                     Left reason -> void $ postString reason

matchingAction :: String -> [(String, BotAction)] -> Maybe ([String], BotAction)
matchingAction t as = helper t $ map (mapFst R.mkRegex) as
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

mapFst :: (a -> c) ->  (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
