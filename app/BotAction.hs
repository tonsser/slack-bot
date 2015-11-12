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
import qualified Data.Text as T
import Control.Monad.Trans.Except
import Text.Regex
import UrlHelpers
import SlackAPI
import qualified NewRelic as NR
import DateParse
import Giphy
import qualified Github as GH
import qualified DuckDuckGo as DG
import HttpHelpers
import EvalRuby

type CommandMatches = [String]
type PostToSlack = String -> IO (Either ErrorMsg ())
type ErrorMsg = String
type AccessToken = Text

type UnauthenticatedActionHandler = (PostToSlack -> CommandMatches -> IO (Either ErrorMsg ()))
type AuthenticatedActionHandler = (AccessToken -> PostToSlack -> CommandMatches -> IO (Either ErrorMsg ()))

data BotAction = UnauthticatedAction UnauthenticatedActionHandler
               | AuthenticatedAction AuthenticatedActionHandler

actions :: [(String, BotAction)]
actions = [ ("what time is it", UnauthticatedAction getTime)
          , ("tell me a joke", UnauthticatedAction randomJoke)
          , ("flip a coin", UnauthticatedAction flipCoin)
          , ("help", UnauthticatedAction help)
          , ("set a timer to (.+) minutes", UnauthticatedAction timer)
          , ("who should pickup lunch", UnauthticatedAction pickupLunch)
          , ("cat me", UnauthticatedAction cat)
          , ("gif me (.*)", UnauthticatedAction gif)
          , ("whos there", AuthenticatedAction whosThere)
          , ("is it time for coffee", UnauthticatedAction coffeeTime)
          , ("whats a functor", UnauthticatedAction whatsFunctor)
          , ("whats an applicative", UnauthticatedAction whatsApplicative)
          , ("whats a monad", UnauthticatedAction whatsMonad)
          , ("tell me about (.*)", UnauthticatedAction tellMeAbout)
          , ("api metrics from (.*) to (.*)", UnauthticatedAction apiMetrics)
          , ("api errors from (.*) to (.*)", UnauthticatedAction apiErrors)
          , ("request feature (.*)", UnauthticatedAction requestFeature)
          , ("ruby (.+)", UnauthticatedAction ruby)

          -- `authenticate` is overriden by `SlackHelpers` but has to be here
          -- otherwise it wont show in `help`
          , ("authenticate", UnauthticatedAction doNothing)
          ]

ruby :: UnauthenticatedActionHandler
ruby postToSlack args = do
    let command = intercalate "+" args
    response <- evalRuby command
    case response of
      Right output -> postToSlack output
      Left e -> postToSlack "There was an error..." >> postToSlack (show e)

requestFeature :: UnauthenticatedActionHandler
requestFeature postToSlack args = do
    let pharse = intercalate "+" args
    response <- GH.createIssue pharse
    case response of
      Right () -> postToSlack "Noted!"
      Left e -> postToSlack "There was an error..." >> postToSlack (show e)

apiErrors :: UnauthenticatedActionHandler
apiErrors postToSlack [from, to] = postNewRelicData ls NR.getErrorCount from to postToSlack
    where ls = [ ("Error count", NR.errorCount) ]
apiErrors postToSlack _ = postToSlack "Parse failed"

apiMetrics :: UnauthenticatedActionHandler
apiMetrics postToSlack [from, to] = postNewRelicData ls NR.getMetricsReport from to postToSlack
    where ls = [ ("Average response time", NR.averageReponseTime)
               , ("Calls per minute", NR.callsPerMinute)
               , ("Call count", NR.callCount)
               , ("Min response time", NR.minResponseTime)
               , ("Max response time", NR.maxResponseTime)
               , ("Average exclusive time", NR.averageExclusionTime)
               , ("Average value", NR.averageValue)
               , ("Total call time per minute", NR.totalCallTimePerMinute)
               , ("Requests per minute", NR.requestsPerMinute)
               , ("Standard deviation", NR.standardDeviation)
               ]
apiMetrics postToSlack _ = postToSlack "Parse failed"

postNewRelicData :: [(Text, a -> Text)]
                    -> (DateRepresentation -> DateRepresentation -> IO (Either GenericException a))
                    -> String
                    -> String
                    -> (String -> IO b)
                    -> IO b
postNewRelicData ls fetchData from to postToSlack = do
    void $ postToSlack "1 second..."
    report <- runExceptT $ do
      fromDate <- ExceptT $ parseNaturalLanguageDate $ pack from
      toDate <- ExceptT $ parseNaturalLanguageDate $ pack to
      ExceptT $ fetchData fromDate toDate
    case report of
      Left e -> postToSlack $ show e
      Right r -> postToSlack $ intercalate "\n" $ map (\(t, f) -> T.unpack t ++ ": " ++ T.unpack (f r)) ls

tellMeAbout :: UnauthenticatedActionHandler
tellMeAbout postToSlack args = do
    let phrase = intercalate "+" args
    response <- DG.tellMeAbout phrase
    case response of
      Left e -> postToSlack (show e)
      Right answer -> postToSlack answer

whatsFunctor :: UnauthenticatedActionHandler
whatsFunctor postToSlack _ = postToSlack $ mconcat [ "class Functor (f :: * -> *) where\n"
                                                   , "  fmap :: (a -> b) -> f a -> f b"
                                                   ]

whatsApplicative :: UnauthenticatedActionHandler
whatsApplicative postToSlack _ = postToSlack $ mconcat [ "class Functor f => Applicative (f :: * -> *) where\n"
                                                       , "  pure :: a -> f a\n"
                                                       , "  (<*>) :: f (a -> b) -> f a -> f b"
                                                       ]

whatsMonad :: UnauthenticatedActionHandler
whatsMonad postToSlack _ = postToSlack $ mconcat [ "class Applicative m => Monad (m :: * -> *) where\n"
                                                 , "  (>>=) :: m a -> (a -> m b) -> m b\n"
                                                 , "  return :: a -> m a"
                                                 ]

coffeeTime :: UnauthenticatedActionHandler
coffeeTime postToSlack _ = postToSlack "Let me check on that" >> postToSlack "Yes it is!"

whosThere :: AuthenticatedActionHandler
whosThere accessToken postToSlack _ = do
    users <- usersList accessToken
    case users of
      Nothing -> postToSlack "Problem with slack"
      Just users' -> let x = mconcat ["Online users: ", list]
                           where list = intercalate ", " $ map slackUserName users'
                     in postToSlack x

getTime :: UnauthenticatedActionHandler
getTime postToSlack _ = do
    date <- removeExtraSpaces <$> runProcess "date"
    let text = mconcat ["The current time is ", date, ", at least where I am"]
    postToSlack text

doNothing :: UnauthenticatedActionHandler
doNothing _ _ = return $ Right ()

httpGet :: String -> IO String
httpGet url = BS.unpack . LBS.toStrict <$> HTTP.simpleHttp url

gif :: UnauthenticatedActionHandler
gif postToSlack phrase = do
    let query = intercalate "+" phrase
    urlM <- gifMe query
    case urlM of
      Left e -> postToSlack $ show e
      Right url -> postToSlack url

-- TODO: Make a module that wraps this, and uses safeHttpLbs
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

safeNth :: Int -> [a] -> Maybe a
safeNth _ [] = Nothing
safeNth 0 (x:_) = Just x
safeNth n (_:xs) = safeNth (n - 1) xs

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ safeNth idx xs

randomJoke :: UnauthenticatedActionHandler
randomJoke postToSlack _ = sample jokes >>= postToSlack . fromJust
  where
    jokes = [ "What is mario's favorite type of pants? Denim denim denim."
            , "What did the Buddhist say to the hot dog vendor? Make me one with everything."
            , "What did the grape say after the elephant sat on it? Nothing, it just let out a little whine."
            , "What do you call it when a dinosaur crashes his car? Tyrannosaurus Wrecks"
            , "How does a lion like his meat? ROAR"
            , "What did the fish say when he ran into the wall? Dam."
            , "What do you do when you see a spaceman? Park your car, man!"
            , "What did one hat say to another? You stay here, I'll go on a head!"
            , "What do cats eat for breakfast? Mice Krispies!"
            , "What do pigs write with? A pig pen!"
            , "Why couldn't Dracula's wife get to sleep? Because of his coffin."
            , "Did you hear about the fire at the circus? It was IN TENTS."
            , "What does a ghost wear when it's raining outside? Boooooo-ts!"
            , "What game would you play with a wombat? Wom."
            , "A baby seal walks into a club."
            , "A dyslexic man walks into a bra."
            , "Bigfoot is blurry, it's not the photographer's fault."
            , "The worst time to have a heart attack is during a game of charades"
            , "My friend has difficulty sleeping, but I can do it with my eyes closed."
            , "I quit my job at the helium factory, I refuse to be spoken to in that tone."
            , "Get me a crocodile sandwich, and make it snappy!"
            , "Two cannibals are eating a clown. One turns to the other and says, 'Does this taste funny to you?'"
            ]

runProcess :: String -> IO String
runProcess cmd = SP.readProcess cmd [] []

removeExtraSpaces :: String -> String
removeExtraSpaces = fix (L.replace "  " " ")
