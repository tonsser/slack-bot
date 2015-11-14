module SlackAPI
    ( oauthAccess
    , usersList
    , endPointWithParams
    , ApiMethod (..)
    , SlackUser (..)
    , postResponseToSlack
    )
  where

import Import hiding (httpLbs, newManager)
import Network.HTTP.Conduit
import UrlHelpers
import Data.Aeson
import System.Environment
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import Control.Lens
import qualified Data.Vector as V
import SlackTypes
import EnvHelpers

data SlackUser = SlackUser
               { slackUserName :: String
               } deriving (Show)

instance FromJSON SlackUser where
    parseJSON (Object v) = SlackUser <$>
                           v .: "name"
    parseJSON _ = mzero

usersList :: Text -> IO (Maybe [SlackUser])
usersList accessToken = runMaybeT $ do
    response <- MaybeT $ runEndPoint UsersList [ ("token", accessToken)
                                               , ("presence", "1")
                                               ]
    allMembers <- MaybeT $ return $ response ^? key "members" . _Array
    let presentMembers = filter userIsPresent allMembers
    MaybeT $ return $ parseSlackUsers presentMembers

userIsPresent :: Value -> Bool
userIsPresent user = presence == Just "active"
    where
      presence = user ^? key "presence" . _String

parseSlackUsers :: Vector Value -> Maybe [SlackUser]
parseSlackUsers = sequence . V.toList . V.map (successOrNothing . fromJSON)

successOrNothing :: Result a -> Maybe a
successOrNothing (Success x) = Just x
successOrNothing _ = Nothing

-- TODO: Remove redirectUri parameter, its not used
oauthAccess :: Text -> Maybe Text -> IO (Maybe Value)
oauthAccess code _redirectId = runEndPoint OauthAccess [("code", code)]

clientId :: IO (Maybe Text)
clientId = fmap pack <$> lookupEnv "TONSS_SLACK_CLIENT_ID"

clientSecret :: IO (Maybe Text)
clientSecret = fmap pack <$> lookupEnv "TONSS_SLACK_CLIENT_SECRET"

apiRoot :: Text
apiRoot = "https://slack.com/api/"

data ApiMethod = OauthAccess
               | UsersList

instance Show ApiMethod where
    show OauthAccess = "oauth.access"
    show UsersList = "users.list"

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

postResponseToSlack :: SlackResponseDestination -> Text -> IO ()
postResponseToSlack destination text = do
    initReq <- slackUrl >>= parseUrl
    let
      res = SlackResponse text destination

      body = encode res

      httpReq :: Request
      httpReq = initReq { method = "POST"
                        , requestBody = RequestBodyLBS body
                        }
    man <- newManager tlsManagerSettings
    void $ httpLbs httpReq man

slackUrl :: IO String
slackUrl = lookupEnvironmentVariable "TONSS_SLACK_INCOMING_WEBHOOK_URL"
