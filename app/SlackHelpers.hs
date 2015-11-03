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
import BotAction

processRequest :: SlackRequest -> IO (Maybe SlackResponse)
processRequest req =
    let
      text :: String
      text = T.unpack $ textWithoutTriggerWord req

      match :: Maybe ([String], BotAction)
      match = matchingAction text actions
    in case match of
         Nothing -> return Nothing
         Just (matches, a) -> (fmap . fmap)
                                (\res -> SlackResponse res)
                                (a matches)

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
