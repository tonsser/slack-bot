module BotAction
  ( actions
  , BotAction (..)
  , fix
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

-- type BotAction = PostToSlack -> CommandMatches -> IO (Either ErrorMsg ())
type CommandMatches = [String]
type PostToSlack = String -> IO (Either ErrorMsg ())
type ErrorMsg = String
type AccessToken = String

type UnauthenticatedActionHandler = (PostToSlack -> CommandMatches -> IO (Either ErrorMsg ()))
type AuthenticatedActionHandler = (AccessToken -> PostToSlack -> CommandMatches -> IO (Either ErrorMsg ()))

data BotAction = UnauthticatedAction UnauthenticatedActionHandler
               | AuthenticatedAction AuthenticatedActionHandler

actions :: [(String, BotAction)]
actions = [ ("what time is it", UnauthticatedAction getTime)
          , ("tell me a joke", UnauthticatedAction randomJoke)
          , ("flip a coin", UnauthticatedAction flipCoin)
          , ("help", UnauthticatedAction help)
          , ("set a timer to (.+) minutes?", UnauthticatedAction timer)
          , ("who should pickup lunch", UnauthticatedAction pickupLunch)
          , ("cat me", UnauthticatedAction cat)
          , ("whos there", AuthenticatedAction whosThere)
          -- `authenticate` is overriden by `SlackHelpers` but has to be here
          -- otherwise it wont show in `help`
          , ("authenticate", UnauthticatedAction doNothing)
          ]

whosThere :: AuthenticatedActionHandler
whosThere accessToken postToSlack _ = postToSlack $ "Your access token is " ++ accessToken

getTime :: UnauthenticatedActionHandler
getTime postToSlack _ = do
    date <- removeExtraSpaces <$> runProcess "date"
    let text = mconcat ["The current time is ", date, ", at least where I am"]
    postToSlack text

doNothing :: UnauthenticatedActionHandler
doNothing _ _ = return $ Right ()

httpGet :: String -> IO String
httpGet url = BS.unpack . LBS.toStrict <$> HTTP.simpleHttp url

cat :: UnauthenticatedActionHandler
cat postToSlack _ = do
    resp <- httpGet "http://thecatapi.com/api/images/get?format=xml"
    case matchRegex (mkRegex "<url>(.*)</url>") resp of
      Just [url] -> postToSlack url
      _ -> err "Something went wrong"

pickupLunch :: UnauthenticatedActionHandler
pickupLunch postToSlack _ =
    -- interact with slack api to find list of online users
    -- pick two at random and return those
    postToSlack "Not implemented yet"

timer :: UnauthenticatedActionHandler
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

err :: String -> IO (Either String a)
err reason = return $ Left reason

flipCoin :: UnauthenticatedActionHandler
flipCoin postToSlack _ = sample ["heads", "tails"] >>= postToSlack . fromJust

help :: UnauthenticatedActionHandler
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

randomJoke :: UnauthenticatedActionHandler
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
