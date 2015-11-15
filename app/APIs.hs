module APIs
    ( instagramHashTagSearch
    , instagramRecentUserMedia
    , xkcd
    , searchRubyGems
    , RubyGem(..)
    )
  where

import Import hiding (replace)
import HttpHelpers
import Data.Aeson.Lens
import Control.Lens
import Data.List.Utils
import Control.Monad.Trans.Except
import EnvHelpers
import qualified Data.Vector as V
import Data.Aeson

searchRubyGems :: String -> IO (Either GenericException [RubyGem])
searchRubyGems query = do
    let
      resultToMaybe :: Result a -> Maybe a
      resultToMaybe (Success x) = Just x
      resultToMaybe _ = Nothing

      parseRubyGems :: Value -> [RubyGem]
      parseRubyGems (Array v) = catMaybes $ V.toList $ V.map (resultToMaybe . fromJSON) v
      parseRubyGems _ = []

      req = mkReq { reqDefUrl = "https://rubygems.org/api/v1/search.json"
                  , reqDefQueryParams = Just [("query", query)]
                  }
    response <- runJsonRequest $ parseRubyGems <$> fetchJson req
    case response of
      Right xs -> return $ Right xs
      Left e -> return $ Left e

data RubyGem = RubyGem
             { gemHomepageUri :: String
             , gemInfo :: String
             , gemProjectUri :: String
             , gemVersionDownloads :: Int
             , gemName :: String
             } deriving (Show)

instance FromJSON RubyGem where
    parseJSON (Object v) = RubyGem <$>
                           v .: "homepage_uri" <*>
                           v .: "info" <*>
                           v .: "project_uri" <*>
                           v .: "version_downloads" <*>
                           v .: "name"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

xkcd :: Int -> IO (Either GenericException String)
xkcd n = do
    let
      req = mkReq { reqDefUrl = "http://xkcd.com/" ++ show n ++ "/info.0.json"
                  }
    response <- runJsonRequest $ (\v -> cs <$> v ^? key "img" . _String) <$> fetchJson req
    return $ unwrapOrError response

getClientId :: IO String
getClientId = lookupEnvironmentVariable "TONSS_INSTAGRAM_CLIENT_ID"

instagramHashTagSearch :: String -> Int -> IO (Either GenericException [String])
instagramHashTagSearch tag count = do
    clientId <- getClientId
    let
      tag' = replace " " "-" tag
      req = mkReq { reqDefUrl = "https://api.instagram.com/v1/tags/" ++ tag' ++ "/media/recent"
                  , reqDefQueryParams = Just [ ("client_id", clientId)
                                             , ("count", show count)
                                             ]
                  }
    response <- runJsonRequest $ parseInstagramMediaArray <$> fetchJson req
    return $ unwrapOrError response

parseInstagramMediaArray :: Value -> Maybe [String]
parseInstagramMediaArray v = do
    v' <- v ^? key "data" . _Array
    let y = sequence $ V.toList $ V.map (\x -> x ^? key "images" . key "standard_resolution" . key "url" . _String) v'
    fmap cs <$> y

instagramRecentUserMedia :: String -> Int -> IO (Either GenericException [String])
instagramRecentUserMedia query count = do
    clientId <- getClientId
    runExceptT $ do
      instaId <- ExceptT $ instagramGetUserId query
      let
        req = mkReq { reqDefUrl = "https://api.instagram.com/v1/users/" ++ instaId ++ "/media/recent"
                    , reqDefQueryParams = Just [ ("client_id", clientId)
                                               , ("count", show count)
                                               ]
                    }
      ExceptT $ unwrapOrError <$> runJsonRequest (parseInstagramMediaArray <$> fetchJson req)

instagramGetUserId :: String -> IO (Either GenericException String)
instagramGetUserId query = do
    clientId <- getClientId
    let
      query' = replace " " "-" query
      req = mkReq { reqDefUrl = "https://api.instagram.com/v1/users/search"
                  , reqDefQueryParams = Just [ ("client_id", clientId)
                                             , ("q", query')
                                             ]
                  }

      parse :: Value -> Maybe String
      parse v = do
          dat <- v ^? key "data" . _Array
          fs <- dat V.!? 0
          cs <$> fs ^? key "id" . _String
    response <- runJsonRequest $ parse <$> fetchJson req
    return $ unwrapOrError response

unwrapOrError :: Either GenericException (Maybe b) -> Either GenericException b
unwrapOrError response =
    case response of
      Right (Just x) -> Right x
      Right Nothing -> Left $ GenericException "Error parsing json"
      Left e -> Left e
