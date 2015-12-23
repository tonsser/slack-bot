module Handler.LocalBot where

import Import
import LocalRequests
import EnvHelpers

getLocalBotR :: Handler Html
getLocalBotR = defaultLayout $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
    $(widgetFile "local-bot")

postLocalBotR :: Handler String
postLocalBotR = do
    httpBasicAuth
    t <- lookupPostParam "text"
    case t of
      Nothing -> return "Missing text param"
      Just t' -> do
        let sampleRequest = makeLocalRequest { localRequestText = t' }
        responses <- liftIO $ runResponsesLocally sampleRequest
        return $ intercalate "\n" responses

httpBasicAuth :: Handler ()
httpBasicAuth = do
  request <- waiRequest
  auth <- lookupBasicAuth
  case auth of
    Nothing -> do
      setHeader "WWW-Authenticate" "Basic Realm=\"My Realm\""
      permissionDenied "Authentication required"
    Just (username, password) -> do
      realUsername <- liftIO $ cs <$> lookupEnvironmentVariable "TONSS_USERNAME"
      realPassword <- liftIO $ cs <$> lookupEnvironmentVariable "TONSS_PASSWORD"
      unless (username == realUsername && password == realPassword) $ do
          setHeader "WWW-Authenticate" "Basic Realm=\"My Realm\""
          permissionDenied "Authentication required"
