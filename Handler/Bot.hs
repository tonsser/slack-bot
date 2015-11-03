module Handler.Bot where

import Import
import SlackTypes
import Control.Monad.Trans.Maybe
import qualified SlackHelpers as SH

getBotR :: Handler Value
getBotR = do
    let json = toJSON $ ImageReferenceUrl "hit"
    return json

postBotR :: Handler Value
postBotR = do
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
    case req of
      Nothing -> error "Missing params"
      Just req' -> do
        res <- liftIO $ SH.processRequest req'
        case res of
          Nothing -> error "Couldn't handle that"
          Just res' -> return $ toJSON res'
