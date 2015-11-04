module BotAction
  ( actions
  , getTime
  , BotAction
  ) where

import Import
import qualified Data.List.Utils as L
import System.Random
import qualified System.Process as SP
import Prelude (read)
import Control.Concurrent

type BotAction = (String -> IO (Either String ())) -> [String] -> IO (Either String ())

actions :: [(String, BotAction)]
actions = [ ("what time is it", getTime)
          -- , ("tell me a joke", randomJoke)
          -- , ("flip a coin", flipCoin)
          , ("help", help)
          , ("set a timer to (.*) seconds", timer)
          ]

timer :: BotAction
timer postToSlack [time] = do
    threadDelay $ (read time) * 1000000
    postToSlack "Times up!"
timer _ _ = err "Couldn't parse time"

getTime :: BotAction
getTime postToSlack _ = do
    date <- runProcess "date"
    let text = mconcat ["The current time is ", date]
    postToSlack text

err :: String -> IO (Either String a)
err reason = return $ Left reason

-- flipCoin :: BotAction
-- flipCoin _ _ = do
--     result <- sample ["heads", "tails"]
--     return $ result

help :: BotAction
help postToSlack _ = postToSlack $ mconcat doc
  where
    commands :: [String]
    commands = map fst (sortBy (compare `on` fst) actions)

    doc :: [String]
    doc = [ "Here are the commands I know about: ", "\n"
          , intercalate "\n" $ map ("- " ++) commands
          ]

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x:_) = Just x
nth n (_:xs) = nth (n - 1) xs

sample :: [a] -> IO (Maybe a)
sample xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ nth idx xs

-- randomJoke :: BotAction
-- randomJoke _ _ = sample [ "one"
--                       , "two"
--                       , "three"
--                       ]

runProcess :: String -> IO String
runProcess cmd = removeExtraSpaces <$> SP.readProcess cmd [] []

removeExtraSpaces :: String -> String
removeExtraSpaces = fix (L.replace "  " " ")

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = let x' = f x
              in if x == x'
                   then x'
                   else fix f x'
