module HttpHelpers
    ( safeHttpLbs
    , GenericException(..),
    )
  where

import Import hiding (httpLbs, newManager)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as LBS

data GenericException = GenericException String
                      | WrappedHttpExcpetion HttpException
                      deriving (Show)

instance Exception GenericException

safeHttpLbs :: Request -> IO (Either GenericException (Response LBS.ByteString))
safeHttpLbs req = do
    man <- newManager defaultManagerSettings
    response <- try $ httpLbs req man
    case response of
      Right x -> return $ Right x
      Left e -> return $ Left $ WrappedHttpExcpetion e
