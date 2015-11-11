module HttpHelpers
    ( performRequest
    )
  where

import Import hiding (httpLbs, newManager)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

performRequest :: Request -> IO Text
performRequest req = do
    man <- newManager defaultManagerSettings
    pack . BS.unpack . LBS.toStrict <$> responseBody <$> httpLbs req man

