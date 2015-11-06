module SlackHelpersSpec (spec) where

import TestImport
import qualified BotAction as BA
import qualified SlackHelpers as SH
import SlackTypes
import qualified Text.Regex as R

nop :: BA.BotAction
nop _ _ = return $ Right ()

sampleActions :: [(String, BA.BotAction)]
sampleActions = [ ("foobar", nop)
                , ("tell me about (.*)", nop)
                , ("what do you (.+) about (.+)", nop)
                ]

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
    let request' = sampleSlackReqest { slackRequestText = "bot hi there"
                                     , slackRequestTriggerWord = "bot"
                                     }

    describe "textWithoutTriggerWord" $ do
      it "finds the matching action" $ do
        let text = SH.textWithoutTriggerWord request'
        text `shouldBe` "hi there"

    describe "matchingAction" $ do
      it "returns the action that matches" $ do
        let commandArgs = fst <$> SH.matchingAction "foobar" sampleActions
        commandArgs `shouldBe` (Just [])

      it "returns the action that matches the pattern" $ do
        let commandArgs = fst <$> SH.matchingAction "tell me about hest ko" sampleActions
        commandArgs `shouldBe` (Just ["hest ko"])

      it "returns the action that matches the pattern with multiple words" $ do
        let commandArgs = fst <$> SH.matchingAction "what do you think about hest" sampleActions
        commandArgs `shouldBe` (Just ["think", "hest"])

      it "returns Nothing if no action matches" $ do
        let commandArgs = fst <$> SH.matchingAction "hest" sampleActions
        commandArgs `shouldBe` Nothing

    describe "toQueryParams" $ do
      it "converts a list of params to queries" $ do
        let query = SH.toQueryParams [("foo", "bar"), ("baz", "qux")]
        "?foo=bar&baz=qux" `shouldBe` query

    describe "regex matching" $ do
      it "matches" $ do
        let regex = R.mkRegex "set a timer to (.+) seconds"
        (Just ["2"]) `shouldBe` (R.matchRegex regex "set a timer to 2 seconds")

      it "matches" $ do
        let regex = R.mkRegex "set a timer to (.+) seconds?"
        (Just ["1"]) `shouldBe` (R.matchRegex regex "set a timer to 1 second")
