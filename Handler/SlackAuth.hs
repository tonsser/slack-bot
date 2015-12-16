module Handler.SlackAuth where

import Import
import qualified Data.Text as T
import UrlHelpers
import SlackAPI

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
    slackClientIdM <- liftIO clientId
    case slackClientIdM of
      Nothing -> error "Missing slack client id in ENV variables"
      Just slackClientId -> do
        let
          (callback, _) = renderRoute SlackAuthCallbackR
          redirectUri = concatPaths $ T.pack appRoot : callback
          queryParams = toQueryParams [ ("team", teamId)
                                      , ("redirect_uri", redirectUri)
                                      , ("client_id", slackClientId)
                                      , ("scope", "users:read")
                                      ]
        url <- toTextUrl $ "https://slack.com/oauth/authorize" `T.append` queryParams
        redirect url
