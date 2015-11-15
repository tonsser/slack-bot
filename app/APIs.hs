module APIs
    ( instagramHashTagSearch
    , instagramRecentUserMedia
    )
  where

import Import hiding (replace)
import HttpHelpers
import Data.Aeson.Lens
import Control.Lens
import qualified Data.Vector as V
import Data.List.Utils
import Control.Monad.Trans.Except
import EnvHelpers

getClientId :: IO String
getClientId = lookupEnvironmentVariable "TONSS_INSTAGRAM_CLIENT_ID"

instagramHashTagSearch :: String -> IO (Either GenericException [String])
instagramHashTagSearch tag = do
    clientId <- getClientId
    let
      tag' = replace " " "-" tag
      req = mkReq { reqDefUrl = "https://api.instagram.com/v1/tags/" ++ tag' ++ "/media/recent"
                  , reqDefQueryParams = Just [ ("client_id", clientId)
                                             ]
                  }
    response <- runJsonRequest $ parseInstagramMediaArray <$> fetchJson req
    return $ unwrapOrError response

parseInstagramMediaArray :: Value -> Maybe [String]
parseInstagramMediaArray v = do
    v' <- v ^? key "data" . _Array
    let y = sequence $ V.toList $ V.map (\x -> x ^? key "images" . key "standard_resolution" . key "url" . _String) v'
    fmap cs <$> y

instagramRecentUserMedia :: String -> IO (Either GenericException [String])
instagramRecentUserMedia query = do
    clientId <- getClientId
    runExceptT $ do
      instaId <- ExceptT $ instagramGetUserId query
      let
        req = mkReq { reqDefUrl = "https://api.instagram.com/v1/users/" ++ instaId ++ "/media/recent"
                    , reqDefQueryParams = Just [ ("client_id", clientId)
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
