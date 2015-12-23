module Handler.Bot where

import Import
import SlackTypes
import Control.Monad.Trans.Maybe
import BotResponder
import qualified Data.Set as Set
import qualified BotAction as B
import Data.Maybe (fromJust)
import qualified Data.Text as T
import SlackAPI
import EnvHelpers

getBotR :: Handler Html
getBotR = defaultLayout $ $(widgetFile "homepage")

postBotR :: Handler Value
postBotR = do
    tokenError <- verifyToken
    case tokenError of
      Just e -> return e
      Nothing -> do
        reqOrErr <- buildSlackRequestFromParams
        case reqOrErr of
          Left missing -> do
            putStrLn $ mconcat [ "Missing params: "
                               , T.intercalate ", " missing
                               ]
            return "Something went wrong, check logs"
          Right req -> do
            accessToken <- getAccessTokenForUserWithSlackId $ slackRequestUserId req
            responses <- liftIO $ responsesForRequest accessToken req
            case responses of
              Right rs -> do
                singleResponse <- liftIO $ postResponses rs req
                case singleResponse of
                  Nothing -> return $ toJSON ([] :: [String])
                  Just response -> respondWith response
              Left err -> respondWith err

verifyToken :: Handler (Maybe Value)
verifyToken = do
    requiredToken <- liftIO getVerificationToken
    t <- lookupPostParam "token"
    case t of
      Nothing -> return $ Just $ toJSON $ OutgoingWebhookResponse "Missing verification token param"
      Just actualToken ->
        if requiredToken == actualToken
          then return Nothing
          else return $ Just $ toJSON $ OutgoingWebhookResponse "Verification token mismatch"

getVerificationToken :: IO Text
getVerificationToken = cs <$> lookupEnvironmentVariable "TONSS_SLACK_VERIFICATION_TOKEN"

-- | Finding acces token

getAccessTokenForUserWithSlackId :: Text -> Handler (Maybe Text)
getAccessTokenForUserWithSlackId slackUserId = runMaybeT $ do
    user <- MaybeT $ findUserWithFilters [UserSlackUserId ==. slackUserId]
    return $ userAccessToken user

findUserWithFilters :: [Filter User] -> Handler (Maybe User)
findUserWithFilters filters = (fmap . fmap) entityVal (runDB $ selectFirst filters [])

-- | Posting response back to Slack

respondWith :: (Monad m, ConvertibleStrings a Text) => a -> m Value
respondWith = return . toJSON . OutgoingWebhookResponse . cs

postResponses :: (BotRequest r) => [B.ResponseRunner] -> r -> IO (Maybe String)
postResponses responses req = case responses of
                                [response] -> liftM Just (response return)
                                _ -> do
                                  evalAll (\x -> postResponseToRequest req x >> return "") responses
                                  return Nothing

postResponseToRequest :: (BotRequest r) => r -> String -> IO ()
postResponseToRequest req txt = void $ postResponseToSlack (destinationForRequest req) $ T.pack txt

destinationForRequest :: (BotRequest r) => r -> SlackResponseDestination
destinationForRequest = SlackResponseChannel . requestChannelName

evalAll :: (Monad m, MonoFoldable c, Element c ~ (t -> m a)) => t -> c -> m ()
evalAll f = foldr (\x -> (>>) (x f)) (return ())

-- | Building slack request

type MissingParams = [Text]

buildSlackRequestFromParams :: (MonadHandler m) => m (Either MissingParams SlackRequest)
buildSlackRequestFromParams = do
    let
      requiredParams :: [Text]
      requiredParams = [ "token" , "team_id" , "team_domain"
                       , "channel_id" , "channel_name" , "timestamp"
                       , "user_id" , "user_name" , "text" , "trigger_word"
                       ]

    presentParams <- catMaybes <$> mapM lookupPostParam requiredParams
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
