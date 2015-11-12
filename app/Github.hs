module Github
    ( createIssue
    , issues
    , FeatureRequest(..)
    , issueNumber
    , issueUrl
    , issueTitle
    , issueBody
    )
  where

import Import hiding (httpLbs, newManager)
import System.Environment
import Network.URI.Encode (encodeTextToBS)
import Data.CaseInsensitive
import HttpHelpers
import Data.Aeson hiding (json)
import qualified Data.ByteString.Lazy as LBS

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
        params = [ ("access_token", Just accessToken) ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "GET"
                                              , requestHeaders = [("User-Agent" :: CI ByteString, "Tonsser-Slack-Bot")]
                                              }
    body <- fmap responseBody <$> safeHttpLbs req
    return $ body >>= parseGithubIssues

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
