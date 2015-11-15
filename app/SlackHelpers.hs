module SlackHelpers where

import Import hiding (group)
import SlackTypes
import qualified Data.Text as T
import qualified Text.Regex as R
import qualified BotAction as BA
import SlackAPI
import qualified Data.List.Utils as L

findMatchAndProcessRequest :: Maybe Text -> SlackRequest -> IO ()
findMatchAndProcessRequest accessToken req =
    case matchingAction (commandForRequest req) BA.actions of
      Nothing -> void $ postResponseToRequest req "Sorry but I don't understand that. Type \"bot help\" to get a list of things I understand."
      Just (matches, action) -> processRequest matches action accessToken req

postResponseToRequest :: SlackRequest -> String -> IO (Either a ())
postResponseToRequest req txt = do
  postResponseToSlack (destinationForRequest req) $ T.pack txt
  return $ Right ()

destinationForRequest :: SlackRequest -> SlackResponseDestination
destinationForRequest = SlackResponseChannel . slackRequestChannelName

commandForRequest :: SlackRequest -> String
commandForRequest = replaceFunkyChars . cs . textWithoutTriggerWord

replaceFunkyChars :: String -> String
replaceFunkyChars str = foldr (uncurry L.replace) str replacements
  where
    replacements = [ ("“", "\"")
                   , ("”", "\"")
                   , ("‘", "'")
                   , ("’", "'")
                   , ("–", "--")
                   , ("—", "---")
                   , ("…", "...")
                   ]

processRequest :: [String] -> BA.BotAction -> Maybe Text -> SlackRequest -> IO ()
processRequest matches action accessToken req =
    if userOfRequestInGroup req (BA.accessGroup action)
      then processPossiblyAuthenticatedAction (BA.actionHandler action) req accessToken matches
      else void $ postResponseToRequest req "Sorry but you don't have permission to do that"

processPossiblyAuthenticatedAction :: BA.ActionHandler
                                   -> SlackRequest
                                   -> Maybe Text
                                   -> [String]
                                   -> IO ()
processPossiblyAuthenticatedAction (BA.Unauthenticated action) req _ matches = do
    res <- action (postResponseToRequest req) matches req
    case res of
      Right () -> return ()
      Left reason -> void $ postResponseToRequest req reason
processPossiblyAuthenticatedAction (BA.Authenticated _) req Nothing _ =
    void $ postResponseToRequest req "Authenticate required, type: \"bot authenticate\""
processPossiblyAuthenticatedAction (BA.Authenticated action) req (Just token) matches = do
    res <- action token (postResponseToRequest req) matches req
    case res of
      Right () -> return ()
      Left reason -> void $ postResponseToRequest req reason

userOfRequestInGroup :: SlackRequest -> BA.AccessGroup -> Bool
userOfRequestInGroup _ (BA.Everyone) = True
userOfRequestInGroup req (BA.Developers) =
    contains (slackRequestUsername req) [ "davidpdrsn"
                                        , "planck"
                                        , "karlo"
                                        , "tkrogsboll"
                                        , "jacobesp"
                                        ]

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains x (y : ys) = (x == y) || contains x ys

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
  where regex = R.mkRegex $ "^" ++ actionCommandToRegex pat ++ "\\?*$"

textWithoutTriggerWord :: SlackRequest -> Text
textWithoutTriggerWord req = T.pack $ R.subRegex pat (T.unpack $ slackRequestText req) ""
  where
    triggerWord = T.unpack $ slackRequestTriggerWord req
    pat = R.mkRegex $ "^" ++ triggerWord ++ " "

actionCommandToRegex :: String -> String
actionCommandToRegex str = R.subRegex (R.mkRegex "\\{[^}]+\\}") str "(.+)"
