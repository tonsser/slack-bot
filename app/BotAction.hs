module BotAction
  ( actions
  , getTime
  , BotAction
  ) where

import Import
import System.Random
import qualified Data.Text as T
import qualified System.Process as SP

type BotAction = [String] -> IO (Maybe Text)

actions :: [(String, BotAction)]
actions = [ ("what time is it", getTime)
          , ("tell me a joke", randomJoke)
          , ("help", help)
          ]

help :: BotAction
help _ = concatText doc
  where
    commands :: [Text]
    commands = map (T.pack . fst) actions

    doc :: [Text]
    doc = [ "Here are the commands I know about: ", "\n"
          , T.intercalate "\n" $ map (T.append "- ") commands
          ]

getTime :: BotAction
getTime _ = do
    date <- runProcess "date"
    concatText ["The current datetime is ", date]

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 [x] = Just x
nth n (_:xs) = nth (n - 1) xs

sample :: [a] -> IO (Maybe a)
sample xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ nth idx xs

randomJoke :: BotAction
randomJoke _ = sample [ "How many programmers does it take to screw in a light bulb? None, thats a hardware problem"
                      ]

runProcess :: Text -> IO Text
runProcess cmd = T.pack <$> SP.readProcess (T.unpack cmd) [] []

concatText :: (Monad m, Monoid a) => [a] -> m (Maybe a)
concatText = return . Just . mconcat
