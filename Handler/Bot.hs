module Handler.Bot where

import Import
import SlackTypes
import Control.Monad.Trans.Maybe
import BotAction
import qualified Data.List.Utils as L
import qualified Data.Text as T
import qualified Text.Regex as R

getBotR :: Handler Value
getBotR = do
    let json = toJSON $ ImageReferenceUrl "hit"
    return json

processRequest :: SlackRequest -> IO (Maybe SlackResponse)
processRequest req =
    let
      text :: String
      text = T.unpack $ textWithoutTriggerWord req

      actionsWithRegex :: [(R.Regex, BotAction)]
      actionsWithRegex = map (`mapFst` R.mkRegex) actions

      match :: Maybe ([String], BotAction)
      match = matchingAction text actionsWithRegex

      channel :: Maybe ResponseDestination
      channel = Just $ ResponseDestinationChannel $ slackRequestChannelName req
    in case match of
         Nothing -> return Nothing
         Just (matches, a) -> (fmap . fmap)
                                (\res -> SlackResponse res Nothing channel)
                                (a matches)

matchingAction :: String -> [(R.Regex, BotAction)] -> Maybe ([String], BotAction)
matchingAction _ [] = Nothing
matchingAction text ((regex, h) : rest) =
    let
      match :: Maybe [String]
      match = R.matchRegex regex text
    in case match of
         Nothing -> matchingAction text rest
         Just matches -> Just (matches, h)

mapFst :: (a, b) -> (a -> c) -> (c, b)
mapFst (a, b) f = (f a, b)

textWithoutTriggerWord :: SlackRequest -> Text
textWithoutTriggerWord req = T.pack $ L.replace (triggerWord req) "" $ T.unpack $ slackRequestText req
  where
    triggerWord :: SlackRequest -> String
    triggerWord = T.unpack . slackRequestTriggerWord

postBotR :: Handler Value
postBotR = do
    let lookup' = MaybeT . lookupPostParam
    req <- runMaybeT $ SlackRequest <$> lookup' "token"
                                    <*> lookup' "team_id"
                                    <*> lookup' "team_domain"
                                    <*> lookup' "channel_id"
                                    <*> lookup' "channel_name"
                                    <*> lookup' "timestamp"
                                    <*> lookup' "user_id"
                                    <*> lookup' "user_name"
                                    <*> lookup' "text"
                                    <*> lookup' "trigger_word"
    case req of
      Nothing -> error "Missing params"
      Just req' -> do
        res <- liftIO $ processRequest req'
        case res of
          Nothing -> error "Couldn't handle that"
          Just res' -> return $ toJSON res'
