module NewRelic
  ( averageReponseTime
  , callsPerMinute
  , callCount
  , minResponseTime
  , maxResponseTime
  , averageExclusionTime
  , averageValue
  , totalCallTimePerMinute
  , requestsPerMinute
  , standardDeviation
  , MetricsReport
  , getMetricsReport
  , getErrorCount
  , ErrorCount
  , errorCount
  )
  where

import Import hiding (httpLbs, newManager)
import Text.Regex
import System.Environment
import Data.CaseInsensitive
import Network.URI.Encode (encodeTextToBS)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import DateParse
import qualified Data.Text as T
import HttpHelpers
import Control.Monad.Trans.Except

data MetricsReport = MetricsReport
                   { averageReponseTime :: Text
                   , callsPerMinute :: Text
                   , callCount :: Text
                   , minResponseTime :: Text
                   , maxResponseTime :: Text
                   , averageExclusionTime :: Text
                   , averageValue :: Text
                   , totalCallTimePerMinute :: Text
                   , requestsPerMinute :: Text
                   , standardDeviation :: Text
                   }

data ErrorCount = ErrorCount
                { errorCount :: Text
                }

getMetricsReport :: DateRepresentation -> DateRepresentation -> IO (Either GenericException MetricsReport)
getMetricsReport fromRep toRep = runExceptT $ do response <- ExceptT $ runNewRelicRequest "Agent/MetricsReported/count" fromRep toRep
                                                 ExceptT $ return $ parseMetricsReport response

getErrorCount :: DateRepresentation -> DateRepresentation -> IO (Either GenericException ErrorCount)
getErrorCount fromRep toRep = runExceptT $ do response <- ExceptT $ runNewRelicRequest "Errors/all" fromRep toRep
                                              ExceptT $ return $ parseErrorCount response

runNewRelicRequest :: ByteString -> DateRepresentation -> DateRepresentation -> IO (Either GenericException Text)
runNewRelicRequest param fromRep toRep = do
    appId <- unpack <$> getAppId
    apiKey <- encodeTextToBS <$> getApiKey
    let from = T.unpack $ dateRep fromRep
        to = T.unpack $ dateRep toRep
        url = "https://api.newrelic.com/v2/applications/" ++ appId ++ "/metrics/data.xml"
        params = [ ("from", Just $ BS.pack from)
                 , ("to", Just $ BS.pack to)
                 , ("summarize", Just "true")
                 , ("names[]", Just param)
                 ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "GET"
                                              , requestHeaders = [("X-Api-Key" :: CI ByteString, apiKey)]
                                              }
    fmap (byteStringToText . responseBody) <$> safeHttpLbs req

byteStringToText :: LBS.ByteString -> Text
byteStringToText = pack . BS.unpack . LBS.toStrict

parseMetricsReport :: Text -> Either GenericException MetricsReport
parseMetricsReport xml = MetricsReport <$> parse xml "average_response_time"
                                       <*> parse xml "calls_per_minute"
                                       <*> parse xml "call_count"
                                       <*> parse xml "min_response_time"
                                       <*> parse xml "max_response_time"
                                       <*> parse xml "average_exclusive_time"
                                       <*> parse xml "average_value"
                                       <*> parse xml "total_call_time_per_minute"
                                       <*> parse xml "requests_per_minute"
                                       <*> parse xml "standard_deviation"

parseErrorCount :: Text -> Either GenericException ErrorCount
parseErrorCount xml = ErrorCount <$> parse xml "error_count"

parse :: Text -> Text -> Either GenericException Text
parse xml key = case matchRegex regex (unpack xml) of
                  Just [x] -> Right $ pack x
                  _ -> Left $ GenericException "There was an error (parsing xml)"
  where
    regex = mkRegex pat
    pat = mconcat ["<", unpack key, ">(.*)</", unpack key, ">"]

getAppId :: IO Text
getAppId = pack <$> fromMaybe (error "Missing env var TONSS_NEW_RELIC_APP_ID") <$> lookupEnv "TONSS_NEW_RELIC_APP_ID"

getApiKey :: IO Text
getApiKey = pack <$> fromMaybe (error "Missing env var TONSS_NEW_RELIC_API_KEY") <$> lookupEnv "TONSS_NEW_RELIC_API_KEY"

-- Sample xml from newrelic
--
-- <?xml version="1.0" encoding="UTF-8"?>
-- <metric_data_response>
--   <metric_data>
--     <from>2015-11-02T14:48:43+00:00</from>
--     <to>2015-11-09T00:00:00+00:00</to>
--     <metrics>
--       <metric>
--         <name>Agent/MetricsReported/count</name>
--         <timeslices>
--           <timeslice>
--             <from>2015-11-02T15:00:00+00:00</from>
--             <to>2015-11-09T00:00:00+00:00</to>
--             <values>
--               <average_response_time>111</average_response_time>
--               <calls_per_minute>6.99</calls_per_minute>
--               <call_count>64144</call_count>
--               <min_response_time>21.0</min_response_time>
--               <max_response_time>972</max_response_time>
--               <average_exclusive_time>111</average_exclusive_time>
--               <average_value>0.111</average_value>
--               <total_call_time_per_minute>0.775</total_call_time_per_minute>
--               <requests_per_minute>6.99</requests_per_minute>
--               <standard_deviation>129</standard_deviation>
--             </values>
--           </timeslice>
--         </timeslices>
--       </metric>
--     </metrics>
--   </metric_data>
-- </metric_data_response>
--
-- Sample error rate response
--
-- <?xml version="1.0" encoding="UTF-8"?>
-- <metric_data_response>
--   <metric_data>
--     <from>2014-04-01T00:00:00+00:00</from>
--     <to>2015-04-01T23:35:00+00:00</to>
--     <metrics>
--       <metric>
--         <name>Errors/all</name>
--         <timeslices>
--           <timeslice>
--             <from>2014-04-03T23:35:00+00:00</from>
--             <to>2015-04-01T23:35:00+00:00</to>
--             <values>
--               <error_count>0</error_count>
--             </values>
--           </timeslice>
--         </timeslices>
--       </metric>
--     </metrics>
--   </metric_data>
-- </metric_data_response>
