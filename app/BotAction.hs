module BotAction
  ( actions
  , BotAction
  ) where

import Import
import Data.Maybe (fromJust)
import qualified Data.List.Utils as L
import System.Random
import qualified System.Process as SP
import Control.Concurrent
import Text.Read
import qualified Network.HTTP.Conduit as HTTP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Text.Regex

type BotAction = (String -> IO (Either String ())) -> [String] -> IO (Either String ())

actions :: [(String, BotAction)]
actions = [ ("what time is it", getTime)
          , ("tell me a joke", randomJoke)
          , ("flip a coin", flipCoin)
          , ("help", help)
          , ("set a timer to (.+) minutes?", timer)
          , ("who should pickup lunch", pickupLunch)
          , ("cat me", cat)
          -- `authenticate` is overriden by `SlackHelpers` but has to be here
          -- otherwise it wont show in `help`
          , ("authenticate", doNothing)
          ]

doNothing :: BotAction
doNothing _ _ = return $ Right ()

httpGet :: String -> IO String
httpGet url = BS.unpack . LBS.toStrict <$> HTTP.simpleHttp url

cat :: BotAction
cat postToSlack _ = do
    resp <- httpGet "http://thecatapi.com/api/images/get?format=xml"
    case matchRegex (mkRegex "<url>(.*)</url>") resp of
      Just [url] -> postToSlack url
      _ -> err "Something went wrong"

pickupLunch :: BotAction
pickupLunch postToSlack _ =
    -- interact with slack api to find list of online users
    -- pick two at random and return those
    postToSlack "Not implemented yet"

timer :: BotAction
timer postToSlack [time] = do
    let
      t :: Maybe Int
      t = readMaybe time
    case t of
      Nothing -> err "Couldn't parse time"
      Just t' -> do
        _ <- postToSlack "Starting timer!"
        threadDelay $ t' * 60 * 1000000
        postToSlack "Times up!"
timer _ _ = err "Couldn't parse time"

getTime :: BotAction
getTime postToSlack _ = do
    date <- removeExtraSpaces <$> runProcess "date"
    let text = mconcat ["The current time is ", date, ", at least where I am"]
    postToSlack text

err :: String -> IO (Either String a)
err reason = return $ Left reason

flipCoin :: BotAction
flipCoin postToSlack _ = sample ["heads", "tails"] >>= postToSlack . fromJust

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
sample [] = return Nothing
sample xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ nth idx xs

randomJoke :: BotAction
randomJoke postToSlack _ = sample ["one", "two", "three"] >>= postToSlack . fromJust

runProcess :: String -> IO String
runProcess cmd = SP.readProcess cmd [] []

removeExtraSpaces :: String -> String
removeExtraSpaces = fix (L.replace "  " " ")

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = let x' = f x
              in if x == x'
                   then x'
                   else fix f x'
