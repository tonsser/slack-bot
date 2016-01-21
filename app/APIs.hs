module APIs
    ( instagramHashTagSearch
    , instagramRecentUserMedia
    , xkcd
    , searchRubyGems
    , RubyGem(..)
    , deleteJohnTonsser
    , todaysLunchMenu
    )
  where

import Control.Lens
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Lens
import Data.Char (isAscii)
import Data.List.Utils
import EnvHelpers
import Data.List.Split
import HttpHelpers
import Import hiding (replace)
import Text.Regex
import qualified Data.Vector as V

todaysLunchMenu :: IO (Either GenericException String)
todaysLunchMenu = do
    let
      req = mkReq { reqDefUrl = "http://e.frokost.dk/Menuplan/RssMenu.aspx"
                  , reqDefQueryParams = Just [("l", "lqmqp2nbVs")]
                  }

      parseTitle :: String -> Maybe String
      parseTitle xml = case matchRegex regex xml of
                         Just [t] -> Just t
                         _ -> Nothing
        where regex = mkRegex "</author>.*<title>(.*)</title>"

      parseDescription :: String -> Maybe String
      parseDescription xml = case matchRegex regex xml of
                               Just [t] -> Just t
                               _ -> Nothing
        where regex = mkRegex "</pubDate>.*<description>(.*)</description>"

      -- TODO: Unescape HTML entities
      unescape :: String -> String
      unescape = (">" `sub` "&gt;" ) .
                 ("<" `sub` "&lt;") .
                 flip (subRegex (mkRegex "&amp;#230;")) "ae" .
                 flip (subRegex (mkRegex "&amp;#248;")) "oe"
        where sub s p i = subRegex (mkRegex p) i s

      subBolds :: String -> String
      subBolds str = subRegex reg str "*"
        where reg = mkRegex "</?b>"

      removeNewlines :: String -> String
      removeNewlines = concat . splitOn "\n"

      parseXML :: String -> Maybe String
      parseXML str = do
        title <- removeNewlines <$> parseTitle str
        desc <- unescape . removeNewlines <$> parseDescription str
        return $ formatMenu title desc

      formatMenu :: String -> String -> String
      formatMenu title desc = mconcat [ "*", title, "*", "\n", "\n"
                                      , desc'
                                      ]
        where desc' = subBolds $ intercalate "\n" $ splitOn "<br/>" desc

    xml <- fmap (parseXML . filter isAscii . cs . responseBody) <$> performRequest req
    case xml of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "Failed parsing RSS menu"
      Right (Just x) -> return $ Right x

deleteJohnTonsser :: IO (Either GenericException ())
deleteJohnTonsser = do
    let
      req = mkReq { reqDefUrl = "https://tonsser-api-staging.herokuapp.com/develop/delete-john-tonsser"
                  , reqDefMethod = Just "DELETE"
                  }
    void <$> performRequest req

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
