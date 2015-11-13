module HttpHelpers ( safeHttpLbs
                   , GenericException(..)
                   , performRequest
                   , RequestDefinition
                   , mkReq
                   , reqDefUrl
                   , reqDefMethod
                   , reqDefQueryParams
                   , reqDefHeaders
                   , reqDefBody
                   )
                   where

import Import hiding (httpLbs, newManager)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI

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

-- TODO: Remove export of this and have everything go through
-- `performRequest
safeHttpLbs :: Request -> IO (Either GenericException (Response LBS.ByteString))
safeHttpLbs req = do
    man <- newManager defaultManagerSettings
    response <- try $ httpLbs req man
    case response of
      Right x -> return $ Right x
      Left e -> return $ Left $ WrappedHttpExcpetion e
