module BaconIpsum
    ( baconIpsum
    )
  where

import Import hiding (httpLbs, newManager)
import HttpHelpers
import Data.Aeson.Lens
import Control.Lens
import qualified Data.Vector as V

baconIpsum :: Int -> IO (Either GenericException [String])
baconIpsum numberOfParagraphs = do
    let req = mkReq { reqDefUrl = "https://baconipsum.com/api/"
                    , reqDefQueryParams = Just [ ("type", "all-meat")
                                               , ("paras", show numberOfParagraphs)
                                               , ("start-with-lorem", "1")
                                               , ("format", "json")
                                               ]
                    }
    result <- runJsonRequest $ parseResponse <$> fetchJson req
    case result of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "There was an error (parsing json). Maybe I don't know anything about that topic"
      Right (Just x) -> return $ Right x

parseResponse :: Value -> Maybe [String]
parseResponse v = catMaybes <$> V.toList . V.map (fmap cs . (^? _String)) <$> v ^? _Array
