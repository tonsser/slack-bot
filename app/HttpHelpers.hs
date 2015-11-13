module HttpHelpers ( performRequest
                   , RequestDefinition
                   , mkReq
                   , reqDefUrl
                   , reqDefMethod
                   , reqDefQueryParams
                   , reqDefHeaders
                   , reqDefBody

                   , JsonResponse
                   , fetchJson
                   , runJsonRequest

                   , GenericException(..)
                   , safeHttpLbs
                   )
                   where

import Import hiding (httpLbs, newManager)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Aeson (decode)
import Control.Monad.Trans.Except

data GenericException = GenericException String
                      | WrappedHttpExcpetion HttpException
                      deriving (Show)

instance Exception GenericException

data RequestDefinition = RequestDefinition
                       { reqDefUrl :: String
                       , reqDefMethod :: Maybe String
                       , reqDefQueryParams :: Maybe [(String, String)]
                       , reqDefHeaders :: Maybe [(String, String)]
                       , reqDefBody :: Maybe String
                       }

mkReq :: RequestDefinition
mkReq = RequestDefinition { reqDefUrl = error "Missing reqDefUrl"
                          , reqDefMethod = Nothing
                          , reqDefQueryParams = Nothing
                          , reqDefHeaders = Nothing
                          , reqDefBody = Nothing
                          }

performRequest :: RequestDefinition -> IO (Either GenericException (Response LBS.ByteString))
performRequest req = do
    initReq <- parseUrl $ reqDefUrl req
    let
      params :: [(ByteString, Maybe ByteString)]
      params = map (\(k, v) -> (cs k, Just $ cs v)) $ fromMaybe [] $ reqDefQueryParams req

      headers :: RequestHeaders
      headers = map (\(k, v) -> (CI.mk $ cs k, cs v)) $ fromMaybe [] $ reqDefHeaders req
    safeHttpLbs $ setQueryString params $ initReq { method = cs $ fromMaybe "GET" $ reqDefMethod req
                                                  , requestHeaders = headers
                                                  , requestBody = RequestBodyLBS $ cs $ fromMaybe "" $ reqDefBody req
                                                  }

newtype JsonResponse a = JsonResponse { runJsonRequest :: IO (Either GenericException a) }

instance Functor JsonResponse where
    fmap f res = JsonResponse $ do
      res' <- runJsonRequest res
      case res' of
        Left e -> return $ Left e
        Right x -> return $ Right $ f x

instance Applicative JsonResponse where
    pure = JsonResponse . return . Right
    f <*> x = JsonResponse $ do f' <- runJsonRequest f
                                x' <- runJsonRequest x
                                return $ do f'' <- f'
                                            x'' <- x'
                                            return $ f'' x''

instance Monad JsonResponse where
    return = pure
    (JsonResponse x) >>= f = JsonResponse $ do x' <- x
                                               case x' of
                                                 Right a -> runJsonRequest $ f a
                                                 Left e -> return $ Left e

fetchJson :: RequestDefinition -> JsonResponse Value
fetchJson req = JsonResponse $ runExceptT $ do
    body <- ExceptT $ fmap responseBody <$> performRequest req
    ExceptT $ return $ case decode body of
      Nothing -> Left $ GenericException "Failed to parse json"
      Just v -> Right v

-- TODO: Remove export of this and have everything go through
-- `performRequest
safeHttpLbs :: Request -> IO (Either GenericException (Response LBS.ByteString))
safeHttpLbs req = do
    man <- newManager defaultManagerSettings
    response <- try $ httpLbs req man
    case response of
      Right x -> return $ Right x
      Left e -> return $ Left $ WrappedHttpExcpetion e
