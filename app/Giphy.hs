module Giphy
    ( gifMe
    )
  where

import Import hiding (httpLbs, newManager)
import System.Environment
import Network.URI.Encode (encodeTextToBS)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Control.Lens hiding (from, to)
import HttpHelpers
import Control.Monad.Trans.Except

gifMe :: String -> IO (Either GenericException String)
gifMe query = do
    url <- runExceptT $ do
      response <- ExceptT $ search query
      let
        json :: Maybe Text
        json = decode response >>= parse

        parse :: Value -> Maybe Text
        parse v = v ^? key "data" . nth 0 . key "images" . key "original" . key "url" . _String
      case json of
        Nothing -> ExceptT $ return $ Left $ GenericException "Error parsing json"
        Just x -> ExceptT $ return $ Right x
    return $ unpack <$> url

search :: String -> IO (Either GenericException LBS.ByteString)
search query = do
    apiKey <- encodeTextToBS <$> getApiKey
    let url = "http://api.giphy.com/v1/gifs/search"
        params = [ ("api_key", Just apiKey)
                 , ("q", Just $ BS.pack query)
                 ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "GET" }
    fmap responseBody <$> safeHttpLbs req

getApiKey :: IO Text
getApiKey = pack <$> fromMaybe (error "Missing env var TONSS_GIPHY_API_KEY") <$> lookupEnv "TONSS_GIPHY_API_KEY"
