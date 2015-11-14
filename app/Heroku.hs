module Heroku
    ( configVars
    )
  where

import Import hiding (httpLbs, newManager)
import EnvHelpers
import Data.Aeson.Lens
import Control.Lens hiding (from, to)
import HttpHelpers

-- configVars :: String -> IO (Either GenericException String)
configVars app = do
    let req = mkReq { reqDefUrl = "https://api.heroku.com/apps/" ++ app ++ "/config-vars"
                    , reqDefQueryParams = Just [
                                               ]
                    , reqDefHeaders = Just [ ("Accept", "application/vnd.heroku+json; version=3")
                                           ]
                    }
    runJsonRequest $ fetchJson req
