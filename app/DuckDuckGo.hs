module DuckDuckGo
    ( tellMeAbout
    )
  where

import Import hiding (httpLbs, newManager)
import HttpHelpers
import Data.Aeson.Lens
import Control.Lens

tellMeAbout :: String -> IO (Either GenericException String)
tellMeAbout query = do
    let req = mkReq { reqDefUrl = "http://api.duckduckgo.com/"
                    , reqDefQueryParams = Just [ ("format", "json")
                                               , ("q", query)
                                               ]
                    }
    result <- runJsonRequest $ parseResponse <$> fetchJson req
    case result of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "There was an error (parsing json). Maybe I don't know anything about that topic"
      Right (Just x) -> return $ Right x

parseResponse :: Value -> Maybe String
parseResponse v = abstractAnswer v <|> relatedTopic v
  where
    abstractAnswer :: Value -> Maybe String
    abstractAnswer v = case cs <$> v ^? key "AbstractText" . _String of
                         Just "" -> Nothing
                         x -> x

    relatedTopic :: Value -> Maybe String
    relatedTopic v = cs <$> v ^? key "RelatedTopics" . nth 0 . key "Text" . _String
