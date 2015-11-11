module DateParse
    ( parseNaturalLanguageDate
    , dateRep
    , DateRepresentation
    )
  where

import Import hiding (httpLbs, newManager)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import HttpHelpers

newtype DateRepresentation = DateRepresentation { dateRep :: Text }

parseNaturalLanguageDate :: Text -> IO (Either GenericException DateRepresentation)
parseNaturalLanguageDate phrase = do
    let query = intercalate "+" $ words $ unpack phrase
    initReq <- parseUrl $ "https://natural-language-date-parse.herokuapp.com/?q=" ++ query
    let req = initReq { method = "GET"
                      , checkStatus = \_ _ _ -> Nothing
                      }
    fmap toDateRep <$> safeHttpLbs req

toDateRep :: Response LBS.ByteString -> DateRepresentation
toDateRep res = let body = T.filter (/= '"') $ T.pack . BS.unpack . LBS.toStrict . responseBody $ res
                in DateRepresentation body
