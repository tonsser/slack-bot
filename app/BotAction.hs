module BotAction
    ( actions
    , getTime
    , BotAction
    ) where

import Import
import qualified Data.Text as T
import qualified System.Process as SP

type BotAction = [String] -> IO (Maybe Text)

actions :: [(String, BotAction)]
actions = [ ("what time is it", getTime)
          ]

getTime :: BotAction
getTime _ = do
    date <- runProcess "date"
    concatText ["The current datetime is ", date]

runProcess :: Text -> IO Text
runProcess cmd = T.pack <$> SP.readProcess (T.unpack cmd) [] []

concatText :: (Monad m, Monoid a) => [a] -> m (Maybe a)
concatText = return . Just . mconcat
