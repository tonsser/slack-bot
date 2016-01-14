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

instance Show GenericException where
    show (GenericException str) = str
    show (WrappedHttpExcpetion e) = show e

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

type JsonResponse a = ExceptT GenericException IO a

runJsonRequest :: JsonResponse a -> IO (Either GenericException a)
runJsonRequest = runExceptT

fetchJson :: RequestDefinition -> JsonResponse Value
fetchJson req = do
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
