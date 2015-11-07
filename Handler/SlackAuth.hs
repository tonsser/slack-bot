module Handler.SlackAuth where

import Import
import qualified Data.Text as T
import SlackHelpers

getSlackAuthR :: Handler ()
getSlackAuthR = do
    -- redirect to https://slack.com/oauth/authorize
    --
    -- With params:
    -- client_id    - issued when you created your app (required)
    -- scope        - permissions to request (see below) (required)
    -- redirect_uri - URL to redirect back to (see below) (optional)
    -- state        - unique string to be passed back upon completion
    -- team         - Slack team ID to restrict to (optional)
    --
    -- team is in get params as team_id
    --
    -- TODO: Set client_secret in settings somehow
    -- TODO: Figure out which scopes I'll need
    --
    -- once the flow is over, save the user_id and given token in the
    -- database
    -- on future requests look up the token from the user_id and
    -- use that token for slack api calls
    params <- catMaybes <$> mapM lookupGetParam ["user_id", "team_id"]
    case params of
      [_userId, teamId] -> do
        appRoot <- liftIO getAppRoot
        let
          (callback, _) = renderRoute SlackAuthCallbackR
          redirectUri = concatPaths $ T.pack appRoot : callback
          queryParams = toQueryParams [ ("team", teamId)
                                      , ("redirect_uri", redirectUri)
                                      -- TODO: Store in settings
                                      , ("client_id", "13857431237.13932343510")
                                      , ("scope", "users:read")
                                      ]
        url <- toTextUrl $ "https://slack.com/oauth/authorize" `T.append` queryParams
        redirect url
      _ -> invalidArgs ["user_id", "team_id"]
