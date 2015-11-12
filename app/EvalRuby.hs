module EvalRuby
    ( evalRuby
    )
  where

import Import
import HttpHelpers
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

evalRuby :: String -> IO (Either GenericException String)
evalRuby ruby = do
    let params = [ ("cmd", Just $ BS.pack ruby)
                 ]
    initReq <- parseUrl "http://tryruby.org/levels/1/challenges/0"
    let req = setQueryString params $ initReq { method = "PUT"
                                              , requestHeaders = [("Content-Length" :: CI ByteString, "0")]
                                              }
    result <- runExceptT $ do
      response <- ExceptT $ safeHttpLbs req
      let body = responseBody response
          output = case decode body of
                     Nothing -> Left "Unknown error"
                     Just v -> parseCommand v

          parseCommand :: Value -> Either String String
          parseCommand v = case v ^? key "success" . _Bool of
                             Just True -> case unpack <$> v ^? key "output" . _String of
                                            Nothing -> Left "Unknown error"
                                            Just res -> Right res
                             _ -> case unpack <$> v ^? key "result" . _String of
                                    Nothing -> Left "Unknown error"
                                    Just res -> Left res
      return output
    case result of
      Left e -> return $ Left e
      Right (Left e) -> return $ Left $ GenericException e
      Right (Right x) -> return $ Right x
