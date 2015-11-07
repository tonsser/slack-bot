module Handler.SlackAuthCallback where

import Import
import SlackAPI
import Data.Aeson.Lens
import Control.Lens

lookupParams :: (MonadHandler m) => [Text] -> m (Maybe [Text])
lookupParams keys = do
    values <- mapM lookupGetParam keys
    if length values == length keys
      then return $ Just $ catMaybes values
      else return Nothing

lookUpWithDefault :: (MonadHandler m) => m a -> m (Maybe a) -> m a
lookUpWithDefault def ioMaybeVal = do
    maybeVal <- ioMaybeVal
    case maybeVal of
      Nothing -> def
      Just val -> return val

getSlackAuthCallbackR :: Handler Html
getSlackAuthCallbackR = do
  userId <- lookUpWithDefault (invalidArgs ["slackUserId"]) (lookupSession "slackUserId")
  code <- lookUpWithDefault (invalidArgs ["code"]) (lookupGetParam "code")
  json <- fromMaybe (error "slack error") <$> liftIO (oauthAccess code Nothing)
  let accessToken = fromMaybe (error $ show json) (json ^? key "access_token" . _String)
  _ <- runDB $ insert User { userAccessToken = accessToken, userSlackUserId = userId }
  defaultLayout $(widgetFile "oauth_success")
