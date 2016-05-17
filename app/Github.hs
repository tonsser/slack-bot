module Github
    ( createIssue
    , issues
    , FeatureRequest(..)
    , RepoName(..)
    , issueNumber
    , issueUrl
    , issueTitle
    , issueBody
    , issuePullRequest
    , issueLabels
    , issueComments
    , labelName
    , pullRequestUrl
    , closeIssue
    , commentOnIssue
    , languageBreakdown
    , languagePercentages
    , issueIsPullRequest
    )
  where

import Control.Lens ((^?))
import Control.Monad.Trans.Except
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.CaseInsensitive hiding (map)
import EnvHelpers
import HttpHelpers
import Import hiding (httpLbs, newManager)
import Network.URI.Encode (encodeTextToBS)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

data GithubIssue = GithubIssue
                 { issueNumber :: Int
                 , issueUrl :: String
                 , issueTitle :: String
                 , issueBody :: Maybe String
                 , issuePullRequest :: Maybe GithubPullRequest
                 , issueLabels :: [GithubLabel]
                 , issueCommentsUrl :: String
                 } deriving (Show)

issueIsPullRequest :: GithubIssue -> Bool
issueIsPullRequest issue = isJust $ issuePullRequest issue

data GithubLabel = GithubLabel
                 { labelName :: String
                 } deriving (Show)

data GithubPullRequest = GithubPullRequest
                       { pullRequestUrl :: String
                       } deriving (Show)

instance FromJSON GithubIssue where
    parseJSON (Object v) = GithubIssue <$>
                           v .: "number" <*>
                           v .: "html_url" <*>
                           v .: "title" <*>
                           v .:? "body" <*>
                           v .:? "pull_request" <*>
                           v .: "labels" <*>
                           v .: "comments_url"
    parseJSON _ = mzero

instance FromJSON GithubPullRequest where
    parseJSON (Object v) = GithubPullRequest <$>
                           v .: "html_url"
    parseJSON _ = mzero

instance FromJSON GithubLabel where
    parseJSON (Object v) = GithubLabel <$>
                           v .: "name"
    parseJSON _ = mzero

issues :: String -> IO (Either GenericException [GithubIssue])
issues repo = let url = "https://api.github.com/repos/tonsser/" ++ repo ++ "/issues"
              in getGithubResource "issues" url

issueComments :: GithubIssue -> IO (Either GenericException [GithubComment])
issueComments = getGithubResource "comments" . issueCommentsUrl

getGithubResource :: (FromJSON a) => String -> String -> IO (Either GenericException [a])
getGithubResource debug url = do
    accessToken <- encodeTextToBS <$> getAccessToken
    let params = [ ("access_token", Just accessToken) ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "GET"
                                              , requestHeaders = [("User-Agent" :: CI ByteString, "Tonsser-Slack-Bot")]
                                              }
    body <- fmap responseBody <$> safeHttpLbs req
    return $ body >>= parseGithubJson debug

data GithubUser = GithubUser
                { userLogin :: String
                } deriving (Show)

data GithubComment = GithubComment
                   { commentBody :: String
                   , commentUser :: GithubUser
                   } deriving (Show)

instance FromJSON GithubUser where
    parseJSON (Object v) = GithubUser <$>
                           v .: "login"
    parseJSON _ = mzero

instance FromJSON GithubComment where
    parseJSON (Object v) = GithubComment <$>
                           v .: "body" <*>
                           v .: "user"
    parseJSON _ = mzero

parseGithubJson :: (FromJSON a) => String -> LBS.ByteString -> Either GenericException [a]
parseGithubJson debug json = case decode json of
                               Nothing -> Left $ GenericException $ "Error in parsing: " ++ debug
                               Just x -> Right x

languageBreakdown :: String -> IO (Either GenericException (HashMap String Integer))
languageBreakdown repo = do
    accessToken <- cs <$> getAccessToken
    let req = mkReq { reqDefMethod = Just "GET"
                    , reqDefUrl = "https://api.github.com/repos/tonsser/" ++ repo ++ "/languages"
                    , reqDefHeaders = Just [("User-Agent", "Tonsser-Slack-Bot")]
                    , reqDefQueryParams = Just [("access_token", accessToken)]
                    }
    result <- runJsonRequest $ parseLanguageBreakdown <$> fetchJson req
    case result of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "There was an error (parsing json)"
      Right (Just x) -> return $ Right $ HM.fromList x

languagePercentages :: HashMap String Integer -> HashMap String Float
languagePercentages m = HM.map ((roundToDecimals 2) . (* 100) . percentage) m
  where
    percentage :: Integer -> Float
    percentage bytesInLanguage = fromIntegral bytesInLanguage / fromIntegral total

    total :: Integer
    total = sum $ map snd $ HM.toList m

roundToDecimals n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

parseLanguageBreakdown :: Value -> Maybe [(String, Integer)]
parseLanguageBreakdown (Object o) = parseMap $ HM.toList o
  where
    parseMap :: [(Text, Value)] -> Maybe [(String, Integer)]
    parseMap = mapM parsePair

    parsePair :: (Text, Value) -> Maybe (String, Integer)
    parsePair (k, v) = case fromJSON v of
                         Error _ -> Nothing
                         Success i -> Just (cs k, i)
parseLanguageBreakdown _ = Nothing

closeIssue :: String -> Int -> String -> IO (Either GenericException ())
closeIssue repo number username = runExceptT $ do
  ExceptT $ commentOnIssue repo number ("Closed by " ++ username ++ " via Bot")
  ExceptT $ reallyCloseIssue repo number

reallyCloseIssue :: String -> Int -> IO (Either GenericException ())
reallyCloseIssue repo number = do
    accessToken <- encodeTextToBS <$> getAccessToken
    let url = "https://api.github.com/repos/tonsser/" ++ repo ++ "/issues/" ++ show number
        params = [ ("access_token", Just accessToken)
                 ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "PATCH"
                                              , requestBody = RequestBodyLBS "{\"state\":\"closed\"}"
                                              , requestHeaders = [("User-Agent" :: CI ByteString, "Tonsser-Slack-Bot")]
                                              }
    void <$> safeHttpLbs req

data IssueComment = IssueComment
                  { body :: String
                  }

instance ToJSON IssueComment where
    toJSON s = object [ "body" .= body s
                      ]

commentOnIssue :: String -> Int -> String -> IO (Either GenericException ())
commentOnIssue repo number text = do
    accessToken <- encodeTextToBS <$> getAccessToken
    let url = "https://api.github.com/repos/tonsser/" ++ repo ++ "/issues/" ++ show number ++ "/comments"
        params = [ ("access_token", Just accessToken)
                 ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "POST"
                                              , requestBody = RequestBodyLBS $ encode IssueComment { body = text }
                                              , requestHeaders = [("User-Agent" :: CI ByteString, "Tonsser-Slack-Bot")]
                                              }
    void <$> safeHttpLbs req

parseGithubIssues :: LBS.ByteString -> Either GenericException [GithubIssue]
parseGithubIssues json = case decode json of
                           Nothing -> Left $ GenericException "Error parsing JSON from Github"
                           Just x -> Right x

data FeatureRequest = FeatureRequest
                 { title :: String
                 , username :: String
                 , repoName :: RepoName
                 }

data RepoName = SlackBotRepo
              | ApiRepo

instance Show RepoName where
    show SlackBotRepo = "slack-bot"
    show ApiRepo = "tonsser-api"

instance ToJSON FeatureRequest where
    toJSON s = object [ "title" .= title s
                      , "body" .= ("Requested by " ++ username s)
                      ]

createIssue :: FeatureRequest -> IO (Either GenericException GithubIssue)
createIssue issue = do
    accessToken <- cs <$> getAccessToken
    let req = mkReq { reqDefMethod = Just "POST"
                    , reqDefBody = Just $ cs $ encode issue
                    , reqDefUrl = "https://api.github.com/repos/tonsser/" ++ show (repoName issue) ++ "/issues"
                    , reqDefHeaders = Just [("User-Agent", "Tonsser-Slack-Bot")]
                    , reqDefQueryParams = Just [("access_token", accessToken)]
                    }
    result <- runJsonRequest $ fromJSON <$> fetchJson req
    case result of
      Left e -> return $ Left e
      Right (Error e) -> return $ Left $ GenericException e
      Right (Success x) -> return $ Right x

getAccessToken :: IO Text
getAccessToken = cs <$> lookupEnvironmentVariable "TONSS_GITHUB_ACCESS_TOKEN"
