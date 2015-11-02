module Handler.Bot where

import Import
import SlackTypes
import Control.Monad.Trans.Maybe

getBotR :: Handler Value
getBotR = do
    let json = toJSON $ ImageReferenceUrl "hit"
    return json
    -- defaultLayout $ [whamlet|hi no one|]

postBotR :: Handler Value
postBotR = do
    req <- runMaybeT $ SlackRequest <$> (MaybeT $ lookupPostParam "token")
                                    <*> (MaybeT $ lookupPostParam "team_id")
                                    <*> (MaybeT $ lookupPostParam "team_domain")
                                    <*> (MaybeT $ lookupPostParam "channel_id")
                                    <*> (MaybeT $ lookupPostParam "channel_name")
                                    <*> (MaybeT $ lookupPostParam "timestamp")
                                    <*> (MaybeT $ lookupPostParam "user_id")
                                    <*> (MaybeT $ lookupPostParam "user_name")
                                    <*> (MaybeT $ lookupPostParam "text")
                                    <*> (MaybeT $ lookupPostParam "trigger_word")
    case req of
      Nothing -> error "Missing params"
      Just req' -> return $ toJSON $ req'
