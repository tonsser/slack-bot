module SlackHelpers
    ( textWithoutTriggerWord
    , matchingAction
    , processRequest
    , getAppRoot
    , concatPaths
    , matchActionText
    )
  where

import Import
import SlackTypes
import qualified Data.Text as T
import qualified Text.Regex as R
import UrlHelpers
import qualified BotAction as BA
import BotAction (BotAction)
import SlackAPI

processRequest :: Maybe Text -> SlackRequest -> IO ()
processRequest accessTokenM req =
    let
      text :: String
      text = T.unpack $ textWithoutTriggerWord req

      match :: Maybe ([String], BotAction)
      match = matchingAction text BA.actions

      destination :: SlackResponseDestination
      destination = SlackResponseChannel $ slackRequestChannelName req

      postString txt = do
        postResponseToSlack destination $ T.pack txt
        return $ Right ()
    in case match of
         Nothing -> void $ postString "Sorry but I don't understand that. Type \"bot help\" to get a list of things I understand."
         Just (matches, action) ->
           case BA.actionHandler action of
             BA.Unauthenticated action' -> do
               res <- action' postString matches req
               case res of
                 Right () -> return ()
                 Left reason -> void $ postString reason

             BA.Authenticated action' ->
               case accessTokenM of
                 Nothing -> void $ postString "Authenticate required, type: \"bot authenticate\""
                 Just accessToken -> do
                   res <- action' accessToken postString matches req
                   case res of
                     Right () -> return ()
                     Left reason -> void $ postString reason

matchingAction :: String -> [BA.BotAction] -> Maybe ([String], BA.BotAction)
matchingAction t as = helper t as
  where
    helper :: String -> [BA.BotAction] -> Maybe ([String], BA.BotAction)
    helper _ [] = Nothing
    helper text (action : rest) = case matchActionText t (BA.command action) of
                                         Nothing -> helper text rest
                                         Just matches -> Just (matches, action)

matchActionText :: String -> String -> Maybe [String]
matchActionText text pat = R.matchRegex regex text
  where regex = R.mkRegex $ "^" ++ pat ++ "\\?*$"

textWithoutTriggerWord :: SlackRequest -> Text
textWithoutTriggerWord req = T.pack $ R.subRegex pat (T.unpack $ slackRequestText req) ""
  where
    triggerWord = T.unpack $ slackRequestTriggerWord req
    pat = R.mkRegex $ "^" ++ triggerWord ++ " "
