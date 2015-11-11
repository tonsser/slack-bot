module DuckDuckGo
    ( tellMeAbout
    )
  where

import Import hiding (httpLbs, newManager)
import qualified Data.ByteString.Char8 as BS
import HttpHelpers
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

tellMeAbout :: String -> IO (Either GenericException String)
tellMeAbout query = do
    let params = [ ("q", Just $ BS.pack query)
                 , ("format", Just "json")
                 ]
    initReq <- parseUrl "http://api.duckduckgo.com/"
    let req = setQueryString params $ initReq { method = "GET" }
    result <- runExceptT $ do
      response <- ExceptT $ safeHttpLbs req
      let
        body = responseBody response
        answer = decode body >>= liftM2 (<|>) abstractAnswer relatedTopic

        abstractAnswer :: Value -> Maybe String
        abstractAnswer v = case unpack <$> v ^? key "AbstractText" . _String of
                             Just "" -> Nothing
                             x -> x

        relatedTopic :: Value -> Maybe String
        relatedTopic v = unpack <$> v ^? key "RelatedTopics" . nth 0 . key "Text" . _String
      return answer
    case result of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "There was an error (parsing json). Maybe I don't know anything about that topic"
      Right (Just x) -> return $ Right x
