module SlackTypes where

import Import

data ImageReference = ImageReferenceUrl Text
                    | ImageReferenceEmoji Text

instance ToJSON ImageReference where
  toJSON (ImageReferenceEmoji emoji) = object ["emoji" .= emoji]
  toJSON (ImageReferenceUrl url) = object ["url" .= url]

data ResponseDestination = ResponseDestinationChannel Text
                         | ResponseDestinationUsername Text

instance ToJSON ResponseDestination where
  toJSON (ResponseDestinationChannel channel) = object ["channel" .= channel]
  toJSON (ResponseDestinationUsername username) = object ["username" .= username]

data SlackResponse = SlackResponse
                   { slackResponseText :: Text
                   , slackResponseImage :: Maybe ImageReference
                   , slackResponseChannel :: Maybe ResponseDestination
                   }

instance ToJSON SlackResponse where
  toJSON (SlackResponse text image channel) = object [ "text" .= text
                                                     , "image" .= image
                                                     , "channel" .= channel
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
    toJSON s = object [ "token" .= slackRequestToken s
                      ]

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
