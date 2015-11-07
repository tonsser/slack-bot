module SlackAPI
    ( oauthAccess
    , endPointWithParams
    , ApiMethod (..)
    )
  where

import Import
import Network.HTTP.Conduit
import SlackHelpers
import Data.Aeson
import System.Environment
import qualified Data.Text as T

oauthAccess :: Text -> Maybe Text -> IO (Maybe Value)
oauthAccess code _redirectId = runEndPoint OauthAccess [("code", code)]

clientId :: IO (Maybe Text)
clientId = fmap pack <$> lookupEnv "TONSS_SLACK_CLIENT_ID"

clientSecret :: IO (Maybe Text)
clientSecret = fmap pack <$> lookupEnv "TONSS_SLACK_CLIENT_SECRET"

apiRoot :: Text
apiRoot = "https://slack.com/api/"

data ApiMethod = OauthAccess

instance Show ApiMethod where
    show OauthAccess = "oauth.access"

type Params = [(Text, Text)]

endPointWithParams :: ApiMethod -> Params -> Text
endPointWithParams method params = concatPaths [apiRoot, pack $ show method] `T.append` toQueryParams params

runEndPoint :: ApiMethod -> Params -> IO (Maybe Value)
runEndPoint method params = do
    clientId' <- fromMaybe (error "missing client id") <$> clientId
    clientSecret' <- fromMaybe (error "missing client secret") <$> clientSecret
    let
      params' = params ++ [ ("client_id", clientId')
                          , ("client_secret", clientSecret')
                          ]
      url = unpack $ endPointWithParams method params'
    decode <$> simpleHttp url
