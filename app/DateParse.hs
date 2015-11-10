module DateParse
    ( parseNaturalLanguageDate
    , dateRep
    , DateRepresentation
    )
  where

import Import hiding (httpLbs, newManager)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Conduit
import qualified Data.Text as T

newtype DateRepresentation = DateRepresentation { dateRep :: Text }

parseNaturalLanguageDate :: Text -> IO (Maybe DateRepresentation)
parseNaturalLanguageDate phrase = do
    let query = intercalate "+" $ words $ unpack phrase
    initReq <- parseUrl $ "https://natural-language-date-parse.herokuapp.com/?q=" ++ query
    let req = initReq { method = "GET"
                      , checkStatus = \_ _ _ -> Nothing
                      }
    man <- newManager defaultManagerSettings
    res <- httpLbs req man
    if responseStatus res == ok200
      then do
        let body = T.filter (/= '"') $ T.pack . BS.unpack . LBS.toStrict . responseBody $ res
        return $ Just $ DateRepresentation body
      else return Nothing
