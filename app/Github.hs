module Github
    ( createIssue
    , issues
    , FeatureRequest(..)
    , issueNumber
    , issueUrl
    , issueTitle
    , issueBody
    , closeIssue
    , commentOnIssue
    )
  where

import Import hiding (httpLbs, newManager)
import System.Environment
import Network.URI.Encode (encodeTextToBS)
import Data.CaseInsensitive
import HttpHelpers
import Data.Aeson hiding (json)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Control.Monad.Trans.Except

data GithubIssue = GithubIssue
                 { issueNumber :: Int
                 , issueUrl :: String
                 , issueTitle :: String
                 , issueBody :: Maybe String
                 } deriving (Show)

instance FromJSON GithubIssue where
    parseJSON (Object v) = GithubIssue <$>
                           v .: "number" <*>
                           v .: "html_url" <*>
                           v .: "title" <*>
                           v .:? "body"
    parseJSON _ = mzero

issues :: String -> IO (Either GenericException [GithubIssue])
issues repo = do
    accessToken <- encodeTextToBS <$> getAccessToken
    let url = "https://api.github.com/repos/tonsser/" ++ repo ++ "/issues"
        params = [ ("access_token", Just accessToken)
                 ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "GET"
                                              , requestHeaders = [("User-Agent" :: CI ByteString, "Tonsser-Slack-Bot")]
                                              }
    body <- fmap responseBody <$> safeHttpLbs req
    return $ body >>= parseGithubIssues

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
                 }

instance ToJSON FeatureRequest where
    toJSON s = object [ "title" .= title s
                      , "body" .= ("Requested by " ++ username s)
                      ]

createIssue :: FeatureRequest -> IO (Either GenericException ())
createIssue issue = do
    accessToken <- encodeTextToBS <$> getAccessToken
    let url = "https://api.github.com/repos/tonsser/tonss/issues"
        params = [ ("access_token", Just accessToken) ]
        body = encode issue
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "POST"
                                              , requestBody = RequestBodyLBS body
                                              , requestHeaders = [("User-Agent" :: CI ByteString, "Tonsser-Slack-Bot")]
                                              }
    void <$> safeHttpLbs req

getAccessToken :: IO Text
getAccessToken = pack <$> fromMaybe (error "Missing env var TONSS_GITHUB_ACCESS_TOKEN") <$> lookupEnv "TONSS_GITHUB_ACCESS_TOKEN"
