module Handler.Bot where

import Import
import SlackTypes
import Control.Monad.Trans.Maybe
import qualified SlackHelpers as SH
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import qualified Data.Text as T

getBotR :: Handler Value
getBotR = do
    let json = toJSON $ ImageReferenceUrl "hit"
    return json

type MissingParams = [Text]

buildSlackRequestFromParams :: (MonadHandler m) => m (Either MissingParams SlackRequest)
buildSlackRequestFromParams = do
    let requiredParams = [ "token" , "team_id" , "team_domain"
                         , "channel_id" , "channel_name" , "timestamp"
                         , "user_id" , "user_name" , "text" , "trigger_word"
                         ]
    actualParams <- catMaybes <$> mapM lookupPostParam requiredParams
    let missingParams = listDiff requiredParams actualParams
    case missingParams of
      [] -> liftM Right unsafeBuildRequest
      missing -> return $ Left missing

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
      Left missing -> error $ T.unpack $ mconcat [ "Missing params: "
                                                 , T.intercalate ", " missing
                                                 ]
      Right req -> do
        res <- liftIO $ SH.processRequest req
        case res of
          Nothing -> error "Couldn't handle that"
          Just res' -> return $ toJSON res'
