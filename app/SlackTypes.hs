module SlackTypes where

import Import
import qualified Data.Text as T

data SlackResponse = SlackResponse
                   { slackResponseText :: Text
                   , slackResponseChannel :: Text
                   }

-- "channel": "#other-channel"

instance ToJSON SlackResponse where
  toJSON (SlackResponse text channel) = object [ "text" .= text
                                               -- , "channel" .= ("#" `T.append` channel)
                                               , "channel" .= ("@" `T.append` "david")
                                               ]

data SlackRequest = SlackRequest
                  { slackRequestToken :: Text
                  , slackRequestTeamId :: Text
                  , slackRequestTeamDomain :: Text
                  , slackRequestChannelId :: Text
                  , slackRequestChannelName :: Text
                  , slackRequestTimestamp :: Text
                  , slackRequestUserId :: Text
                  , slackRequestUsername :: Text
                  , slackRequestText :: Text
                  , slackRequestTriggerWord :: Text
                  }

instance ToJSON SlackRequest where
    toJSON s = object [ "token" .= slackRequestToken s ]

-- token=XXXXXXXXXXXXXXXXXX
-- team_id=T0001
-- team_domain=example
-- channel_id=C2147483705
-- channel_name=test
-- timestamp=1355517523.000005
-- user_id=U2147483697
-- user_name=Steve
-- text=googlebot: What is the air-speed velocity of an unladen swallow?
-- trigger_word=googlebot:
