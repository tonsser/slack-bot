module SlackHelpersSpec (spec) where

import TestImport
import qualified BotAction as BA
import qualified SlackHelpers as SH
import SlackTypes
import qualified Text.Regex as R

nop _ _ = return $ Right ()

sampleSlackReqest :: SlackRequest
sampleSlackReqest = SlackRequest { slackRequestToken = "token"
                                 , slackRequestTeamId = "team-id"
                                 , slackRequestTeamDomain = "team-domain"
                                 , slackRequestChannelId = "channel-id"
                                 , slackRequestChannelName = "channel-name"
                                 , slackRequestTimestamp = "timestamp"
                                 , slackRequestUserId = "user-id"
                                 , slackRequestUsername = "username"
                                 , slackRequestText = "text"
                                 , slackRequestTriggerWord = "triggerword"
                                 }

spec :: Spec
spec = do
    let request' = sampleSlackReqest { slackRequestText = "bot request feature Make bot awesome"
                                     , slackRequestTriggerWord = "bot"
                                     }

    describe "textWithoutTriggerWord" $
      it "finds the matching action" $ do
        let text = SH.textWithoutTriggerWord request'
        text `shouldBe` "request feature Make bot awesome"

    describe "textMatchesActionText" $ do
      it "matches when there are no patterns" $
        SH.matchActionText "help" "help" `shouldBe` Just []

      it "doesn't match when there are extra words" $
        SH.matchActionText "help with help" "help" `shouldBe` Nothing

      it "allows extra question marks at the end" $
        SH.matchActionText "what time is it" "what time is it?" `shouldBe` Just []
