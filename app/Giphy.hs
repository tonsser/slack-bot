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
import Network.HTTP.Conduit
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import Control.Lens hiding (from, to)

gifMe :: String -> IO (Maybe String)
gifMe query = do
    url <- runMaybeT $ do
      json <- (MaybeT $ decode <$> search query) :: MaybeT IO Value
      return $ json ^. key "data" . nth 0 . key "images" . key "original" . key "url" . _String
    return $ unpack <$> url

search :: String -> IO LBS.ByteString
search query = do
    apiKey <- encodeTextToBS <$> getApiKey
    let url = "http://api.giphy.com/v1/gifs/search"
        params = [ ("api_key", Just apiKey)
                 , ("q", Just $ BS.pack query)
                 ]
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "GET" }
    man <- newManager defaultManagerSettings
    responseBody <$> httpLbs req man

getApiKey :: IO Text
getApiKey = pack <$> fromMaybe (error "Missing env var TONSS_GIPHY_API_KEY") <$> lookupEnv "TONSS_GIPHY_API_KEY"
