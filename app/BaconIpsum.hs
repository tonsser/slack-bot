module BaconIpsum
    ( baconIpsum
    )
  where

import Import hiding (httpLbs, newManager)
import qualified Data.ByteString.Char8 as BS
import HttpHelpers
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import qualified Data.Vector as V
import qualified Data.Text as T

baconIpsum :: Int -> IO (Either GenericException [String])
baconIpsum numberOfParagraphs = do
    let params = [ ("type", Just "all-meat")
                 , ("paras", Just $ BS.pack $ show numberOfParagraphs)
                 , ("start-with-lorem", Just "1")
                 , ("format", Just "json")
                 ]
    initReq <- parseUrl "https://baconipsum.com/api/"
    let req = setQueryString params $ initReq { method = "GET" }
    result <- runExceptT $ do
      response <- ExceptT $ safeHttpLbs req
      let
        body = responseBody response
        answer = decode body >>= paragraphs

        paragraphs :: Value -> Maybe [String]
        paragraphs v = catMaybes <$> V.toList . V.map (fmap T.unpack . (^? _String)) <$> v ^? _Array
      return answer
    case result of
      Left e -> return $ Left e
      Right Nothing -> return $ Left $ GenericException "There was an error (parsing json). Maybe I don't know anything about that topic"
      Right (Just x) -> return $ Right x
