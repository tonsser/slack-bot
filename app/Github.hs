module Github
    ( createIssue
    , GithubIssue(..)
    )
  where

import Import hiding (httpLbs, newManager)
import System.Environment
import Network.URI.Encode (encodeTextToBS)
import Data.CaseInsensitive
import HttpHelpers
import Data.Aeson

data GithubIssue = GithubIssue
                 { title :: String
                 , username :: String
                 }

instance ToJSON GithubIssue where
    toJSON s = object [ "title" .= title s
                      , "body" .= ("Requested by " ++ username s)
                      ]

createIssue :: GithubIssue -> IO (Either GenericException ())
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
