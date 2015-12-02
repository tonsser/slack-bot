module DataDog
    ( graph
    )
  where

import Import
import EnvHelpers
import HttpHelpers
import Data.Aeson.Lens
import Control.Lens
import DateParse (PosixTime, posixTimeRep)

graph :: String -> PosixTime -> PosixTime -> IO (Either GenericException String)
graph query start end = do
    apiKey <- cs <$> getApiKey
    appKey <- cs <$> getAppKey
    let req = mkReq { reqDefUrl = "https://app.datadoghq.com/api/v1/graph/snapshot"
                    , reqDefQueryParams = Just [ ("api_key", apiKey)
                                               , ("application_key", appKey)
                                               , ("metric_query", query)
                                               , ("start", posixTimeRep start)
                                               , ("end", posixTimeRep end)
                                               ]
                    }
    result <- runJsonRequest $ parseResponse <$> fetchJson req
    case result of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "Failed parsing json"
      Right (Just x) -> return $ Right x

parseResponse :: Value -> Maybe String
parseResponse v = cs <$> v ^? key "snapshot_url" . _String

getApiKey :: IO Text
getApiKey = cs <$> lookupEnvironmentVariable "TONSS_DATADOG_API_KEY"

getAppKey :: IO Text
getAppKey = cs <$> lookupEnvironmentVariable "TONSS_DATADOG_APP_KEY"
