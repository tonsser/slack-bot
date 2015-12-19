module Handler.Bot where

import Import
import SlackTypes
import Control.Monad.Trans.Maybe
import qualified SlackHelpers as SH
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import qualified Data.Text as T

getBotR :: Handler Html
getBotR = defaultLayout $ $(widgetFile "homepage")

type MissingParams = [Text]

buildSlackRequestFromParams :: (MonadHandler m) => m (Either MissingParams SlackRequest)
buildSlackRequestFromParams = do
    let
      requiredParams :: [Text]
      requiredParams = [ "token" , "team_id" , "team_domain"
                       , "channel_id" , "channel_name" , "timestamp"
                       , "user_id" , "user_name" , "text" , "trigger_word"
                       ]

      justIfKeyExists :: (MonadHandler m) => Text -> m (Maybe Text)
      justIfKeyExists key = do
        value <- lookupPostParam key
        case value of
          Nothing -> return Nothing
          Just _ -> return $ Just key

    presentParams <- catMaybes <$> mapM justIfKeyExists requiredParams
    if length presentParams == length requiredParams
      then liftM Right unsafeBuildRequest
      else return $ Left $ listDiff requiredParams presentParams

unsafeBuildRequest :: (MonadHandler m) => m SlackRequest
unsafeBuildRequest = do
    let lookup' = MaybeT . lookupPostParam
    req <- runMaybeT $ SlackRequest <$> lookup' "token"
                                    <*> lookup' "team_id"
                                    <*> lookup' "team_domain"
                                    <*> lookup' "channel_id"
                                    <*> lookup' "channel_name"
                                    <*> lookup' "timestamp"
                                    <*> lookup' "user_id"
                                    <*> lookup' "user_name"
                                    <*> lookup' "text"
                                    <*> lookup' "trigger_word"
    return $ fromJust req

listDiff :: (Ord a) => [a] -> [a] -> [a]
listDiff xs ys = Set.elems $ Set.difference (toSet xs) (toSet ys)
  where
    toSet = foldr Set.insert Set.empty

postBotR :: Handler Value
postBotR = do
    reqOrErr <- buildSlackRequestFromParams
    case reqOrErr of
      Left missing -> do
        putStrLn $ mconcat [ "Missing params: "
                           , T.intercalate ", " missing
                           ]
        return "Something went wrong, check logs"
      Right req -> do
        accessToken <- getAccessTokenForUserWithSlackId $ slackRequestUserId req
        x <- liftIO $ SH.findMatchAndProcessRequest accessToken req
        case x of
          Left () -> return $ toJSON ([] :: [String])
          Right str -> return $ toJSON $ OutgoingWebhookResponse $ cs str

getAccessTokenForUserWithSlackId :: Text -> Handler (Maybe Text)
getAccessTokenForUserWithSlackId slackUserId = runMaybeT $ do
    user <- MaybeT $ findUserWithFilters [UserSlackUserId ==. slackUserId]
    return $ userAccessToken user

findUserWithFilters :: [Filter User] -> Handler (Maybe User)
findUserWithFilters filters = (fmap . fmap) entityVal (runDB $ selectFirst filters [])
