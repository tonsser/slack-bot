module DateParse
    ( parseNaturalLanguageDate
    , dateRep
    , parseNaturalLanguageDateToPosixTime
    , PosixTime
    , DateRepresentation
    , posixTimeRep
    )
  where

import qualified Data.Date.SimpleParse as D
import Data.Time.Format
import HttpHelpers
import Import hiding (httpLbs, newManager)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

newtype DateRepresentation = DateRepresentation { dateRep :: Text }
                             deriving (Show)

newtype PosixTime = PosixTime { posixTimeRep :: String }

parseNaturalLanguageDateToPosixTime :: Text -> IO (Either GenericException PosixTime)
parseNaturalLanguageDateToPosixTime p = fmap PosixTime <$> dateWithFormat "%s" p

parseNaturalLanguageDate :: Text -> IO (Either GenericException DateRepresentation)
parseNaturalLanguageDate p = fmap (DateRepresentation . cs) <$> dateWithFormat format p
    where format = "%Y-%m-%dT%H:%M:%S.000Z"

dateWithFormat f phrase = do
    date <- D.parseDate $ cs phrase
    case date of
      Left e -> return $ Left $ GenericException $ show e
      Right d -> return $ Right $ formatTime defaultTimeLocale f d
