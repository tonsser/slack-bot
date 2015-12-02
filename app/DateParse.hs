module DateParse
    ( parseNaturalLanguageDate
    , dateRep
    , parseNaturalLanguageDateToPosixTime
    , PosixTime
    , DateRepresentation
    , posixTimeRep
    )
  where

import Import hiding (httpLbs, newManager)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import HttpHelpers

newtype DateRepresentation = DateRepresentation { dateRep :: Text }
                             deriving (Show)

newtype PosixTime = PosixTime { posixTimeRep :: String }

parseNaturalLanguageDateToPosixTime :: Text -> IO (Either GenericException PosixTime)
parseNaturalLanguageDateToPosixTime phrase = do
    let query = intercalate "+" $ words $ cs phrase
    initReq <- parseUrl $ "https://natural-language-date-parse.herokuapp.com/?q=" ++ query ++ "&posix_time=true"
    let req = initReq { method = "GET"
                      , checkStatus = \_ _ _ -> Nothing
                      }
    fmap (PosixTime . cs . responseBody) <$> safeHttpLbs req

parseNaturalLanguageDate :: Text -> IO (Either GenericException DateRepresentation)
parseNaturalLanguageDate phrase = do
    let query = intercalate "+" $ words $ cs phrase
    initReq <- parseUrl $ "https://natural-language-date-parse.herokuapp.com/?q=" ++ query
    let req = initReq { method = "GET"
                      , checkStatus = \_ _ _ -> Nothing
                      }
    fmap toDateRep <$> safeHttpLbs req

toDateRep :: Response LBS.ByteString -> DateRepresentation
toDateRep res = let body = T.filter (/= '"') $ cs . responseBody $ res
                in DateRepresentation body
