module Handler.SlackAuth where

import Import
import qualified Data.Text as T
import SlackHelpers

getSlackAuthR :: Handler Html
getSlackAuthR = do
    params <- catMaybes <$> mapM lookupGetParam ["user_id", "team_id"]
    case params of
      [userId, teamId] -> do
        setSession "slackUserId" userId
        user <- runDB $ selectFirst [UserSlackUserId ==. userId] []
        if isJust user
          then defaultLayout $(widgetFile "already_authenticated")
          else redirectToSlack teamId
      _ -> invalidArgs ["user_id", "team_id"]

redirectToSlack :: Text -> Handler a
redirectToSlack teamId = do
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
