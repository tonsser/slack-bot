module Giphy
    ( gifMe
    )
  where

import Import hiding (httpLbs, newManager)
import EnvHelpers
import Data.Aeson.Lens
import Control.Lens hiding (from, to)
import HttpHelpers

gifMe :: String -> IO (Either GenericException String)
gifMe query =
    do
    apiKey <- getApiKey
    let req = mkReq { reqDefUrl = "http://api.giphy.com/v1/gifs/search"
                    , reqDefQueryParams = Just [ ("api_key", cs apiKey)
                                               , ("q", query)
                                               ]
                    }
    url <- runJsonRequest $ parseSearchResult <$> fetchJson req
    case url of
      Right (Just x) -> return $ Right $ cs x
      Right Nothing -> return $ Left $ GenericException "Error parsing json"
      Left e -> return $ Left e

parseSearchResult :: Value -> Maybe Text
parseSearchResult v = v ^? key "data" . nth 0 . key "images" . key "original" . key "url" . _String

getApiKey :: IO Text
getApiKey = cs <$> lookupEnvironmentVariable "TONSS_GIPHY_API_KEY"
