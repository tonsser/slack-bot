module Github
    ( createIssue
    )
  where

import Import hiding (httpLbs, newManager)
import System.Environment
import Network.URI.Encode (encodeTextToBS)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Network.HTTP.Conduit

createIssue :: String -> IO Bool
createIssue title = do
    accessToken <- encodeTextToBS <$> getAccessToken
    let url = "https://api.github.com/repos/tonsser/tonss/issues"
        params = [ ("access_token", Just accessToken) ]
        body = "title=" ++ title
    initReq <- parseUrl url
    let req = setQueryString params $ initReq { method = "POST"
                                              , requestBody = RequestBodyLBS $ BS.fromString body
                                              }
    man <- newManager defaultManagerSettings
    status <- responseStatus <$> httpLbs req man
    if status == status201
      then return True
      else return False

getAccessToken :: IO Text
getAccessToken = pack <$> fromMaybe (error "Missing env var TONSS_GITHUB_ACCESS_TOKEN") <$> lookupEnv "TONSS_GITHUB_ACCESS_TOKEN"
