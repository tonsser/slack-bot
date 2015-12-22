module SlackHelpers where

import Import hiding (group)
import SlackTypes
import qualified Data.Text as T
import qualified Text.Regex as R
import qualified BotAction as BA
import qualified Data.List.Utils as L
import Control.Monad.Trans.State (execStateT)

findMatchAndProcessRequest :: (BotRequest r) => Maybe Text -> r -> IO (Either String [BA.SlackResponseRunner])
findMatchAndProcessRequest accessToken req =
    case matchingAction (commandForRequest req) BA.actions of
      Nothing -> return $ Left "Sorry but I don't understand that. Type \"bot help\" to get a list of things I understand."
      Just (matches, action) -> processRequest matches action accessToken req

-- | Finding the matching action

commandForRequest :: (BotRequest r) => r -> String
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

textWithoutTriggerWord :: (BotRequest r) => r -> Text
textWithoutTriggerWord req = T.pack $ R.subRegex pat (T.unpack $ requestText req) ""
  where
    triggerWord = T.unpack $ requestTriggerWord req
    pat = R.mkRegex $ "^" ++ triggerWord ++ " "

matchingAction :: BotRequest r => String -> [BA.BotAction r] -> Maybe ([String], BA.BotAction r)
matchingAction t as = helper t as
  where
    helper :: BotRequest r => String -> [BA.BotAction r] -> Maybe ([String], BA.BotAction r)
    helper _ [] = Nothing
    helper text (action : rest) = case matchActionText t (BA.command action) of
                                         Nothing -> helper text rest
                                         Just matches -> Just (matches, action)

matchActionText :: String -> String -> Maybe [String]
matchActionText text pat = R.matchRegex regex text
  where regex = R.mkRegex $ "^" ++ actionCommandToRegex pat ++ "\\?*$"

actionCommandToRegex :: String -> String
actionCommandToRegex str = R.subRegex (R.mkRegex "\\{[^}]+\\}") str "(.+)"

-- | Run action and post response into Slack

processRequest :: (BotRequest r) => [String] -> BA.BotAction r -> Maybe Text -> r -> IO (Either String [BA.SlackResponseRunner])
processRequest matches action accessToken req =
    if userOfRequestInGroup req (BA.accessGroup action)
      then processPossiblyAuthenticatedAction (BA.actionHandler action) req accessToken matches
      else return $ Left "Sorry but you don't have permission to do that"

userOfRequestInGroup :: (BotRequest r) => r -> BA.AccessGroup -> Bool
userOfRequestInGroup _ (BA.Everyone) = True
userOfRequestInGroup req (BA.Developers) =
    contains (requestUsername req) [ "davidpdrsn"
                                   , "planck"
                                   , "karlo"
                                   , "tkrogsboll"
                                   , "jacobesp"
                                   ]

processPossiblyAuthenticatedAction :: (BotRequest r)
                                   => BA.ActionHandler r
                                   -> r
                                   -> Maybe Text
                                   -> [String]
                                   -> IO (Either String [BA.SlackResponseRunner])
processPossiblyAuthenticatedAction (BA.Unauthenticated action) req _ matches =
    Right <$> execStateT (action matches req) []
processPossiblyAuthenticatedAction (BA.Authenticated _) _ Nothing _ =
    return $ Left "Authenticate required, type: \"bot authenticate\""
processPossiblyAuthenticatedAction (BA.Authenticated action) req (Just token) matches =
    Right <$> execStateT (action token matches req) []

-- | Helper functions

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains x (y : ys) = (x == y) || contains x ys
