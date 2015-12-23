module Handler.LocalBot where

import Import
import SlackTypes
import LocalRequests

getLocalBotR :: Handler Html
getLocalBotR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
  $(widgetFile "local-bot")

postLocalBotR :: Handler String
postLocalBotR = do
    t <- lookupPostParam "text"
    case t of
      Nothing -> return  "nothing"
      Just t' -> do
        let sampleRequest = makeLocalRequest { localRequestText = cs t' }
        responses <- liftIO $ runResponsesLocally sampleRequest
        return $ intercalate "\n" responses
