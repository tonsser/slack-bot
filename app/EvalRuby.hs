module EvalRuby
    ( evalRuby
    )
  where

import Import
import HttpHelpers
import Data.Aeson.Lens
import Control.Lens

evalRuby :: String -> IO (Either GenericException String)
evalRuby ruby = do
    let req = mkReq { reqDefUrl = "http://tryruby.org/levels/1/challenges/0"
                    , reqDefQueryParams = Just [("cmd", ruby)]
                    , reqDefMethod = Just "PUT"
                    , reqDefHeaders = Just [("Content-Length", "0")]
                    }
    result <- runJsonRequest $ parseCommand <$> fetchJson req
    case result of
      Left e -> return $ Left e
      Right (Left e) -> return $ Left $ GenericException e
      Right (Right x) -> return $ Right x

parseCommand :: Value -> Either String String
parseCommand v = case v ^? key "success" . _Bool of
                   Just True -> case unpack <$> v ^? key "output" . _String of
                                  Nothing -> Left "Unknown error"
                                  Just res -> Right res
                   _ -> case unpack <$> v ^? key "result" . _String of
                          Nothing -> Left "Unknown error"
                          Just res -> Left res
