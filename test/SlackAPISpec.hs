module SlackAPISpec (spec) where

import TestImport
import SlackAPI

spec :: Spec
spec = do
    describe "endPointWithParams" $
      it "returns a valid url" $ do
        let actual = endPointWithParams OauthAccess [("foo", "bar"), ("baz", "qux")]
        actual `shouldBe` "https://slack.com/api/oauth.access?foo=bar&baz=qux"
