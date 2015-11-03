module BotAction
  ( actions
  , getTime
  , BotAction
  ) where

import Import
import qualified Data.List.Utils as L
import System.Random
import qualified Data.Text as T
import qualified System.Process as SP

type BotAction = [String] -> IO (Maybe Text)

actions :: [(String, BotAction)]
actions = [ ("what time is it", getTime)
          , ("tell me a joke", randomJoke)
          , ("flip a coin", flipCoin)
          , ("help", help)
          ]

flipCoin :: BotAction
flipCoin _ = do
    result <- sample ["heads", "tails"]
    return $ result

help :: BotAction
help _ = concatText doc
  where
    commands :: [Text]
    commands = map (T.pack . fst) (sortBy (compare `on` fst) actions)

    doc :: [Text]
    doc = [ "Here are the commands I know about: ", "\n"
          , T.intercalate "\n" $ map (T.append "- ") commands
          ]

getTime :: BotAction
getTime _ = do
    date <- runProcess "date"
    concatText ["The current time is ", date]

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x:_) = Just x
nth n (_:xs) = nth (n - 1) xs

sample :: [a] -> IO (Maybe a)
sample xs = do
    idx <- randomRIO (0, (length xs - 1))
    return $ nth idx xs

randomJoke :: BotAction
randomJoke _ = sample [ "one"
                      , "two"
                      , "three"
                      ]

runProcess :: Text -> IO Text
runProcess cmd = removeExtraSpaces . T.strip . T.pack <$> SP.readProcess (T.unpack cmd) [] []

removeExtraSpaces :: Text -> Text
removeExtraSpaces = T.pack . fix (L.replace "  " " ") . T.unpack

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = let x' = f x
              in if x == x'
                   then x'
                   else fix f x'

concatText :: (Monad m, Monoid a) => [a] -> m (Maybe a)
concatText = return . Just . mconcat
