module BotAction
  ( actions
  , BotAction (..)
  , fix
  , ActionCategory(..)
  , ActionHandler(..)
  , AccessGroup(..)
  ) where

import Import hiding (groupBy, Authenticated)
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
import SlackTypes
import Misc
import qualified BaconIpsum

type CommandMatches = [String]
type PostToSlack = String -> IO (Either ErrorMsg ())
type ErrorMsg = String
type AccessToken = Text

type UnauthenticatedActionHandler = (PostToSlack -> CommandMatches -> SlackRequest -> IO (Either ErrorMsg ()))
type AuthenticatedActionHandler = (AccessToken -> PostToSlack -> CommandMatches -> SlackRequest -> IO (Either ErrorMsg ()))

data ActionHandler = Unauthenticated UnauthenticatedActionHandler
                   | Authenticated AuthenticatedActionHandler

data BotAction = BotAction
               { command :: String
               , actionHandler :: ActionHandler
               , category :: ActionCategory
               , accessGroup :: AccessGroup
               }

data AccessGroup = Developers
                 | Everyone

data ActionCategory = CategoryGithub
                    | CategoryMisc
                    | CategorySilly
                    | CategoryInformation
                    | CategoryUtility
                    | CategoryApiUtility
                    deriving (Eq, Ord)

instance Show ActionCategory where
    show CategoryGithub = "Github"
    show CategoryMisc = "Misc"
    show CategorySilly = "Silly"
    show CategoryInformation = "Information"
    show CategoryUtility = "Utility"
    show CategoryApiUtility = "Api utility"

actions :: [BotAction]
actions = [ BotAction { command = "authenticate"
                      , actionHandler = Unauthenticated authenticateAction
                      , category = CategoryMisc
                      , accessGroup = Everyone
                      }
          , BotAction { command = "tell me a joke"
                      , actionHandler = Unauthenticated randomJoke
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "flip a coin"
                      , actionHandler = Unauthenticated flipCoin
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "help"
                      , actionHandler = Unauthenticated help
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "set a timer to (.+) minutes"
                      , actionHandler = Unauthenticated timer
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "who should pickup lunch"
                      , actionHandler = Unauthenticated pickupLunch
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "cat me"
                      , actionHandler = Unauthenticated cat
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "gif me (.+)"
                      , actionHandler = Unauthenticated gif
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "whos there"
                      , actionHandler = Authenticated whosThere
                      , category = CategoryMisc
                      , accessGroup = Everyone
                      }
          , BotAction { command = "is it time for coffee"
                      , actionHandler = Unauthenticated coffeeTime
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "whats a functor"
                      , actionHandler = Unauthenticated whatsFunctor
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "whats an applicative"
                      , actionHandler = Unauthenticated whatsApplicative
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "whats a monad"
                      , actionHandler = Unauthenticated whatsMonad
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "tell me about (.+)"
                      , actionHandler = Unauthenticated tellMeAbout
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "api metrics from (.+) to (.+)"
                      , actionHandler = Unauthenticated apiMetrics
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "api errors from (.+) to (.+)"
                      , actionHandler = Unauthenticated apiErrors
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "request feature (.+)"
                      , actionHandler = Unauthenticated requestFeature
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "ruby (.+)"
                      , actionHandler = Unauthenticated ruby
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "issues (.+)"
                      , actionHandler = Unauthenticated listIssues
                      , category = CategoryGithub
                      , accessGroup = Everyone
                      }
          , BotAction { command = "close issue #(.+) (.+)"
                      , actionHandler = Unauthenticated closeIssue
                      , category = CategoryGithub
                      , accessGroup = Developers
                      }
          , BotAction { command = "bacon ipsum me"
                      , actionHandler = Unauthenticated baconIpsum
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "bacon me"
                      , actionHandler = Unauthenticated baconMe
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "what time is it"
                      , actionHandler = Unauthenticated getTime
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          ]

baconMe :: UnauthenticatedActionHandler
baconMe postToSlack _ _ = do
    img <- fromJust <$> sample [ "http://static.tumblr.com/efcd314e9fe24722553f594cffa61cfd/g2dhfnu/ArYmlvd7a/tumblr_static_bacon.png"
                               , "http://www.drserenamurray.com/wp-content/uploads/2013/02/1bacon0929.jpg"
                               , "http://www.jdfoods.net/wp-content/uploads/2015/08/original-bacon-image-png1.png"
                               , "https://bacontoday.com/wp-content/uploads/2013/01/Giant-Pile-of-Bacon.jpg"
                               , "http://netstorage.discovery.com/feeds/brightcove/asset-stills/dam/135664103849213930900301197_bacon_tutle_burger.jpg"
                               , "http://i.huffpost.com/gen/1452814/thumbs/o-BACON-570.jpg?5"
                               , "http://dudefoods.com/wordpress/wp-content/uploads/2013/05/BaconWeaveTaco3111.jpg"
                               , "https://timenewsfeed.files.wordpress.com/2013/05/nf_bacon_longevity_0508.jpg?w=480&h=320&crop=1"
                               ]
    postToSlack img

baconIpsum :: UnauthenticatedActionHandler
baconIpsum postToSlack _ _ = do
    n <- fromJust <$> sample [1..7]
    ps <- BaconIpsum.baconIpsum n
    case ps of
      Right x -> postToSlack $ intercalate ". " x
      Left e -> postToSlack "There was an error" >> postToSlack (show e)

closeIssue :: UnauthenticatedActionHandler
closeIssue postToSlack [numberS, repo] req =
    case (readMaybe numberS :: Maybe Int) of
      Nothing -> postToSlack "Couldn't parse number"
      Just number -> do
        response <- GH.closeIssue repo number (T.unpack $ slackRequestUsername req)
        case response of
          Right _ -> postToSlack "Done!"
          Left e -> postToSlack "There was an error" >> postToSlack (show e)
closeIssue postToSlack _ _ = postToSlack "Couldn't parse that message... :("

listIssues :: UnauthenticatedActionHandler
listIssues postToSlack [repo] _ = do
    issues <- GH.issues repo
    case issues of
      Left e -> postToSlack "There was an error" >> postToSlack (show e)
      Right is ->
        let prettyPrintIssue i = intercalate "\n" [ "Title: " ++ GH.issueTitle i
                                                  , "Number: #" ++ show (GH.issueNumber i)
                                                  , "URL: " ++ GH.issueUrl i
                                                  ]
            text = intercalate "\n\n" $ map prettyPrintIssue $ reverse is
        in postToSlack text
listIssues postToSlack _ _ = postToSlack "Repo name cannot contain spaces"

ruby :: UnauthenticatedActionHandler
ruby postToSlack args _ = do
    let command = intercalate "+" args
    response <- evalRuby command
    case response of
      Right output -> postToSlack output
      Left e -> postToSlack "There was an error..." >> postToSlack (show e)

requestFeature :: UnauthenticatedActionHandler
requestFeature postToSlack args slackReq = do
    let pharse = intercalate "+" args
        issue = GH.FeatureRequest { title = pharse
                                  , username = T.unpack $ slackRequestUsername slackReq
                                  }
    response <- GH.createIssue issue
    case response of
      Right () -> postToSlack "Noted!"
      Left e -> postToSlack "There was an error..." >> postToSlack (show e)

apiErrors :: UnauthenticatedActionHandler
apiErrors postToSlack [from, to] _ = postNewRelicData ls NR.getErrorCount from to postToSlack
    where ls = [ ("Error count", NR.errorCount) ]
apiErrors postToSlack _ _ = postToSlack "Parse failed"

apiMetrics :: UnauthenticatedActionHandler
apiMetrics postToSlack [from, to] _ = postNewRelicData ls NR.getMetricsReport from to postToSlack
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
apiMetrics postToSlack _ _ = postToSlack "Parse failed"

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
tellMeAbout postToSlack args _ = do
    let phrase = intercalate "+" args
    response <- DG.tellMeAbout phrase
    case response of
      Left e -> postToSlack (show e)
      Right answer -> postToSlack answer

whatsFunctor :: UnauthenticatedActionHandler
whatsFunctor postToSlack _ _ = postToSlack $ mconcat [ "class Functor (f :: * -> *) where\n"
                                                   , "  fmap :: (a -> b) -> f a -> f b"
                                                   ]

whatsApplicative :: UnauthenticatedActionHandler
whatsApplicative postToSlack _ _ = postToSlack $ mconcat [ "class Functor f => Applicative (f :: * -> *) where\n"
                                                       , "  pure :: a -> f a\n"
                                                       , "  (<*>) :: f (a -> b) -> f a -> f b"
                                                       ]

whatsMonad :: UnauthenticatedActionHandler
whatsMonad postToSlack _ _ = postToSlack $ mconcat [ "class Applicative m => Monad (m :: * -> *) where\n"
                                                 , "  (>>=) :: m a -> (a -> m b) -> m b\n"
                                                 , "  return :: a -> m a"
                                                 ]

coffeeTime :: UnauthenticatedActionHandler
coffeeTime postToSlack _ _ = postToSlack "Let me check on that" >> postToSlack "Yes it is!"

whosThere :: AuthenticatedActionHandler
whosThere accessToken postToSlack _ _ = do
    users <- usersList accessToken
    case users of
      Nothing -> postToSlack "Problem with slack"
      Just users' -> let x = mconcat ["Online users: ", list]
                           where list = intercalate ", " $ map slackUserName users'
                     in postToSlack x

getTime :: UnauthenticatedActionHandler
getTime postToSlack _ _ = do
    date <- removeExtraSpaces <$> runProcess "date"
    let text = mconcat ["The current time is ", date, ", at least where I am"]
    postToSlack text

httpGet :: String -> IO String
httpGet url = BS.unpack . LBS.toStrict <$> HTTP.simpleHttp url

gif :: UnauthenticatedActionHandler
gif postToSlack phrase _ = do
    let query = intercalate "+" phrase
    urlM <- gifMe query
    case urlM of
      Left e -> postToSlack $ show e
      Right url -> postToSlack url

-- TODO: Make a module that wraps this, and uses safeHttpLbs
cat :: UnauthenticatedActionHandler
cat postToSlack _ _ = do
    resp <- httpGet "http://thecatapi.com/api/images/get?format=xml"
    case matchRegex (mkRegex "<url>(.+)</url>") resp of
      Just [url] -> postToSlack url
      _ -> err "Something went wrong"

pickupLunch :: UnauthenticatedActionHandler
pickupLunch postToSlack _ _ =
    -- interact with slack api to find list of online users
    -- pick two at random and return those
    postToSlack "Not implemented yet"

timer :: UnauthenticatedActionHandler
timer postToSlack [time] _ = do
    let
      t :: Maybe Int
      t = readMaybe time
    case t of
      Nothing -> err "Couldn't parse time"
      Just t' -> do
        _ <- postToSlack "Starting timer!"
        threadDelay $ t' * 60 * 1000000
        postToSlack "Times up!"
timer _ _ _ = err "Couldn't parse time"

flipCoin :: UnauthenticatedActionHandler
flipCoin postToSlack _ _ = sample ["heads", "tails"] >>= postToSlack . fromJust

help :: UnauthenticatedActionHandler
help postToSlack _ _ = postToSlack $ mconcat doc
  where
    categories :: [String]
    categories = map categoryParagraph $ groupBy category actions

    categoryParagraph :: (ActionCategory, [BotAction]) -> String
    categoryParagraph (c, as) = intercalate "\n" [ "*" ++ show c ++ ":*"
                                                 , intercalate "\n" $ map command as
                                                 ]

    doc :: [String]
    doc = [ "Here are the commands I know about: "
          , "\n"
          , intercalate "\n\n" categories
          ]

randomJoke :: UnauthenticatedActionHandler
randomJoke postToSlack _ _ = sample jokes >>= postToSlack . fromJust
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

authenticateAction :: UnauthenticatedActionHandler
authenticateAction postToSlack _ req = do
    -- TODO: Don't use environment variables here
    -- TODO: Concat the paths in a nicer way
    appRoot <- getAppRoot
    let
      (slackAuth, _) = mapFst (T.unpack . T.intercalate "/") (renderRoute SlackAuthR)
      params = [ ("team_id", slackRequestTeamId req)
               , ("user_id", slackRequestUserId req)
               ]
      username = slackRequestUsername req
      authUrl = appRoot ++ slackAuth ++ T.unpack (toQueryParams params)
    postResponseToSlack (SlackResponseUsername username) $ T.pack authUrl
    _ <- postToSlack "Check your private messages"
    return $ Right ()

---- Helpers

safeNth :: Int -> [a] -> Maybe a
safeNth _ [] = Nothing
safeNth 0 (x:_) = Just x
safeNth n (_:xs) = safeNth (n - 1) xs

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ safeNth idx xs

err :: String -> IO (Either String a)
err reason = return $ Left reason
