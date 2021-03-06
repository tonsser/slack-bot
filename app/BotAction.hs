module BotAction
  ( actions
  , BotAction (..)
  , fix
  , ActionCategory(..)
  , ActionHandler(..)
  , AccessGroup(..)
  , ResponseState
  , samples
  , ResponseRunner
  ) where

import Import hiding (groupBy, Authenticated, get)
import Data.Maybe (fromJust)
import Data.Char (toLower)
import qualified Data.List.Utils as L
import System.Random
import qualified System.Process as SP
import Control.Concurrent
import Text.Read hiding (get)
import Control.Monad.Trans.Maybe
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
import EvalRuby
import SlackTypes
import Misc
import qualified BaconIpsum
import APIs
import Network.HTTP.Base (urlEncodeVars)
import qualified Data.ByteString.Base64 as B64
import Control.Monad.Trans.State
import qualified DataDog as Dog
import HttpHelpers (GenericException)
import qualified Data.HashMap.Strict as HM

type CommandMatches = [String]
type AccessToken = Text

type UnauthenticatedActionHandler r = CommandMatches -> r -> ResponseState
type AuthenticatedActionHandler r = AccessToken -> CommandMatches -> r -> ResponseState

data (BotRequest r) => ActionHandler r = Unauthenticated (UnauthenticatedActionHandler r)
                                       | Authenticated (AuthenticatedActionHandler r)

data (BotRequest r) => BotAction r = BotAction
                                   { command :: String
                                   , actionHandler :: ActionHandler r
                                   , category :: ActionCategory
                                   , accessGroup :: AccessGroup
                                   }

type ResponseRunner = (String -> IO String) -> IO String
type ResponseState = StateT [ResponseRunner] IO ()

postResponse :: String -> ResponseState
postResponse str = do
    old <- get
    let new actuallyPost = actuallyPost str
    put $ old ++ [new]

insertIO :: IO () -> ResponseState
insertIO action = do
    old <- get
    let new _ = action >> return ""
    put $ old ++ [new]

data AccessGroup = Developers
                 | Everyone
                 deriving (Show)

data ActionCategory = CategoryGithub
                    | CategoryMisc
                    | CategorySilly
                    | CategoryInformation
                    | CategoryUtility
                    | CategoryApiUtility
                    | CategoryInstagram
                    deriving (Eq, Ord)

instance Show ActionCategory where
    show CategoryGithub = "Github"
    show CategoryMisc = "Misc"
    show CategorySilly = "Silly"
    show CategoryInformation = "Information"
    show CategoryUtility = "Utility"
    show CategoryApiUtility = "Api utility"
    show CategoryInstagram = "Instagram"

actions :: (BotRequest r) => [BotAction r]
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
          , BotAction { command = "tell me another joke"
                      , actionHandler = Unauthenticated randomJoke'
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
          , BotAction { command = "set a timer to {number of minutes} minutes"
                      , actionHandler = Unauthenticated timer
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "who should pickup lunch"
                      , actionHandler = Authenticated pickupLunch
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "cat me"
                      , actionHandler = Unauthenticated cat
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "gif me {term}"
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
          , BotAction { command = "tell me about {term}"
                      , actionHandler = Unauthenticated tellMeAbout
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "api metrics from {relative date like -1d} to {relative date like -2h}"
                      , actionHandler = Unauthenticated apiMetrics
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "api errors from {relative date like -3d} to {relative date like -15h}"
                      , actionHandler = Unauthenticated apiErrors
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "request feature {text}"
                      , actionHandler = Unauthenticated requestFeature
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "api open issue {text}"
                      , actionHandler = Unauthenticated createApiIssue
                      , category = CategoryApiUtility
                      , accessGroup = Developers
                      }
          , BotAction { command = "ruby {ruby to evaluate}"
                      , actionHandler = Unauthenticated ruby
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "issues {name of repo}"
                      , actionHandler = Unauthenticated listIssues
                      , category = CategoryGithub
                      , accessGroup = Everyone
                      }
          , BotAction { command = "close issue #{issue number} {repo name}"
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
          , BotAction { command = "insta tag {hash tag} {count}"
                      , actionHandler = Unauthenticated instaHashTagCount
                      , category = CategoryInstagram
                      , accessGroup = Everyone
                      }
          , BotAction { command = "insta tag {hash tag}"
                      , actionHandler = Unauthenticated instaHashTag
                      , category = CategoryInstagram
                      , accessGroup = Everyone
                      }
          , BotAction { command = "insta user {username} {count}"
                      , actionHandler = Unauthenticated instaUserCount
                      , category = CategoryInstagram
                      , accessGroup = Everyone
                      }
          , BotAction { command = "insta user {username}"
                      , actionHandler = Unauthenticated instaUser
                      , category = CategoryInstagram
                      , accessGroup = Everyone
                      }
          , BotAction { command = "xkcd"
                      , actionHandler = Unauthenticated randomXkcd
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "url encode {query}"
                      , actionHandler = Unauthenticated urlEncode'
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "base64 encode {query}"
                      , actionHandler = Unauthenticated base64Encode
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "base64 decode {query}"
                      , actionHandler = Unauthenticated base64Decode
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "rubygems {query}"
                      , actionHandler = Unauthenticated rubyGems
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "how smart are you"
                      , actionHandler = Unauthenticated howSmart
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "what do you think about siri"
                      , actionHandler = Unauthenticated (quickResponse "No comments...")
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "good morning"
                      , actionHandler = Unauthenticated goodMorning
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "how was star wars"
                      , actionHandler = Unauthenticated (quickResponse "I'm not telling!")
                      , category = CategorySilly
                      , accessGroup = Everyone
                      }
          , BotAction { command = "graph me {DataDog query} from {relative date} to {relative date}"
                      , actionHandler = Unauthenticated graphMe
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "delete john tonsser"
                      , actionHandler = Unauthenticated deleteJohnTonsserAction
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "kill john tonsser"
                      , actionHandler = Unauthenticated deleteJohnTonsserAction
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "kill fissirul lohmann"
                      , actionHandler = Unauthenticated deleteFissirulAction
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "whats for lunch"
                      , actionHandler = Unauthenticated whatsForLunch
                      , category = CategoryApiUtility
                      , accessGroup = Everyone
                      }
          , BotAction { command = "how much swift is ios"
                      , actionHandler = Unauthenticated howMuchSwift
                      , category = CategoryInformation
                      , accessGroup = Everyone
                      }
          , BotAction { command = "prs for {repository name}"
                      , actionHandler = Unauthenticated pullRequests
                      , category = CategoryUtility
                      , accessGroup = Everyone
                      }
          ]

pullRequests :: (BotRequest r) => UnauthenticatedActionHandler r
pullRequests [repoName] _ = do
    postResponse "Checking, please wait"
    issues' <- liftIO $ runExceptT $ ExceptT $ GH.issues repoName
    case issues' of
      Left e -> postResponse $ show e
      Right issues -> do
        let pullRequests =
              filter (\(i, _pr) -> let labelNames = map GH.labelName $ GH.issueLabels i
                                   in elem "Status: Review needed" labelNames) $
              foldr (\issue acc -> case GH.issuePullRequest issue of
                                     Nothing -> acc
                                     Just pr -> (issue, pr) : acc) [] issues

            prettyPrintIssuePr i pr = intercalate "\n" [ "Title: " ++ GH.issueTitle i
                                                       , "URL: " ++ GH.pullRequestUrl pr
                                                       ]

            text = intercalate "\n\n" $ map (uncurry prettyPrintIssuePr) pullRequests
        postResponse text
pullRequests _ _ = postResponse "Only one argument name please"

howMuchSwift :: (BotRequest r) => UnauthenticatedActionHandler r
howMuchSwift _ _ = do
    breakdown <- liftIO $ GH.languageBreakdown "tonsser-ios"
    case breakdown of
      Left e -> postResponse $ show e
      Right x -> case HM.lookup "Swift" (GH.languagePercentages x) of
                   Just p -> postResponse $ "iOS is currently " ++ show p ++ "% :swift:"
                   Nothing -> postResponse "No swift found :("

whatsForLunch :: (BotRequest r) => UnauthenticatedActionHandler r
whatsForLunch _ _ = do
    menu <- liftIO todaysLunchMenu
    case menu of
      Left e -> postResponse $ show e
      Right x -> postResponse x

deleteFissirulAction :: (BotRequest r) => UnauthenticatedActionHandler r
deleteFissirulAction _ _ = do
    response <- liftIO deleteFissirul
    case response of
      Right () -> postResponse "Deleted Fissirul"
      Left err -> do postResponse "There was an error, he might already be deleted"
                     postResponse $ show err

deleteJohnTonsserAction :: (BotRequest r) => UnauthenticatedActionHandler r
deleteJohnTonsserAction _ _ = do
    response <- liftIO deleteJohnTonsser
    case response of
      Right () -> postResponse "Deleted John Tonsser"
      Left err -> do postResponse "There was an error, he might already be deleted"
                     postResponse $ show err

quickResponse :: BotRequest r => String -> UnauthenticatedActionHandler r
quickResponse str _ _ = postResponse str

graphMe :: (BotRequest r) => UnauthenticatedActionHandler r
graphMe [query, startDate, endDate] _ = do
    start <- liftIO $ parseNaturalLanguageDateToPosixTime $ cs startDate
    end <- liftIO $ parseNaturalLanguageDateToPosixTime $ cs endDate
    case (start, end) of
      (Right s, Right e) -> do
        response <- liftIO $ Dog.graph query s e
        case response of
          Right graph -> postResponse query >> postResponse graph
          Left err -> postResponse $ show err
      _ -> postResponse "Couldn't parse dates"
graphMe _ _ = postResponse "Couldn't parse arguments"

goodMorning :: (BotRequest r) => UnauthenticatedActionHandler r
goodMorning _ req = postResponse $ "Good morning, " ++ cs username
  where username = requestUsername req

howSmart :: forall r . (BotRequest r) => UnauthenticatedActionHandler r
howSmart _ _ = postResponse sentence
    where
      sentence = intercalate "\n" [ "I'm quite smart should I say so myself"
                                  , "I know about " ++ show (length (actions :: [BotAction r])) ++ " things"
                                  ]

rubyGems :: (BotRequest r) => UnauthenticatedActionHandler r
rubyGems ws _ = do
    let query = intercalate "+" ws
    result <- liftIO $ searchRubyGems query
    case result of
      Right [] -> postResponse "Didn't find any results"
      Right gems -> do
        void $ postResponse "Here is what I found"
        let
          format :: RubyGem -> String
          format gem = intercalate "\n" $ map ($ gem) [ ("*Name*: " ++) . gemName
                                                      , ("*Homepage*: " ++) . gemHomepageUri
                                                      , ("*Project*: " ++) . gemProjectUri
                                                      , ("*Description*: " ++) . gemInfo
                                                      , ("*Downloads of latest version*: " ++) . show . gemVersionDownloads
                                                      ]
        mapM_ (postResponse . (++ "\n") . format) $ safeTake 3 gems
      Left e -> postResponse $ show e

base64Encode :: (BotRequest r) => UnauthenticatedActionHandler r
base64Encode ws _ = postResponse $ cs $ B64.encode $ cs $ unwords ws

base64Decode :: (BotRequest r) => UnauthenticatedActionHandler r
base64Decode ws _ = case B64.decode $ cs $ unwords ws of
                      Left e -> postResponse $ show e
                      Right x -> postResponse $ cs x

urlEncode' :: (BotRequest r) => UnauthenticatedActionHandler r
urlEncode' ws _ = do
    let query = readMaybe (unwords ws) :: Maybe [(String, String)]
    case query of
      Just q -> postResponse $ urlEncodeVars q
      Nothing -> postResponse "Error parsing, input be list of pairs like: [(\"key\", \"value\")]"

randomXkcd :: (BotRequest r) => UnauthenticatedActionHandler r
randomXkcd _ _ = do
    (Just n) <- liftIO $ sample [1..1337]
    url <- liftIO $ xkcd n
    case url of
      Right x -> postResponse x
      Left e -> postResponse $ show e

instaHashTag :: (BotRequest r) => UnauthenticatedActionHandler r
instaHashTag [tag] _ = runInstaAction instagramHashTagSearch tag 1
instaHashTag _ _ = postResponse "Hash tags cannot contain spaces"

instaHashTagCount :: (BotRequest r) => UnauthenticatedActionHandler r
instaHashTagCount [tag, c] _ =
    case readMaybe c :: Maybe Int of
      Just c' -> runInstaAction instagramHashTagSearch tag c'
      Nothing -> postResponse "Couldn't parse count"
instaHashTagCount _ _ = postResponse "Hash tags cannot contain spaces"

instaUser :: (BotRequest r) => UnauthenticatedActionHandler r
instaUser [username] _ = runInstaAction instagramRecentUserMedia username 1
instaUser _ _ = postResponse "Usernames cannot contain spaces"

instaUserCount :: (BotRequest r) => UnauthenticatedActionHandler r
instaUserCount [username, c] _ =
    case readMaybe c :: Maybe Int of
      Just c' -> runInstaAction instagramRecentUserMedia username c'
      Nothing -> postResponse "Couldn't parse count"
instaUserCount _ _ = postResponse "Usernames cannot contain spaces"

runInstaAction :: Show a =>
                  (b -> Int -> IO (Either a [String]))
                  -> b
                  -> Int
                  -> StateT [ResponseRunner] IO ()
runInstaAction f arg c = do
    urls <- liftIO $ f arg c
    case urls of
      Right x -> mapM_ postResponse $ safeTake c x
      Left e -> postResponse "There was an error" >> postResponse (show e)

baconMe :: (BotRequest r) => UnauthenticatedActionHandler r
baconMe _ _ = do
    img <- liftIO $ fromJust <$> sample [ "http://static.tumblr.com/efcd314e9fe24722553f594cffa61cfd/g2dhfnu/ArYmlvd7a/tumblr_static_bacon.png"
                                        , "http://www.drserenamurray.com/wp-content/uploads/2013/02/1bacon0929.jpg"
                                        , "http://www.jdfoods.net/wp-content/uploads/2015/08/original-bacon-image-png1.png"
                                        , "https://bacontoday.com/wp-content/uploads/2013/01/Giant-Pile-of-Bacon.jpg"
                                        , "http://netstorage.discovery.com/feeds/brightcove/asset-stills/dam/135664103849213930900301197_bacon_tutle_burger.jpg"
                                        , "http://i.huffpost.com/gen/1452814/thumbs/o-BACON-570.jpg?5"
                                        , "http://dudefoods.com/wordpress/wp-content/uploads/2013/05/BaconWeaveTaco3111.jpg"
                                        , "https://timenewsfeed.files.wordpress.com/2013/05/nf_bacon_longevity_0508.jpg?w=480&h=320&crop=1"
                                        ]
    postResponse img

baconIpsum :: (BotRequest r) => UnauthenticatedActionHandler r
baconIpsum _ _ = do
    n <- liftIO $ fromJust <$> sample [1..7]
    ps <- liftIO $ BaconIpsum.baconIpsum n
    case ps of
      Right x -> postResponse $ intercalate ". " x
      Left e -> postResponse "There was an error" >> postResponse (show e)

closeIssue :: (BotRequest r) => UnauthenticatedActionHandler r
closeIssue [numberS, repo] req =
    case (readMaybe numberS :: Maybe Int) of
      Nothing -> postResponse "Couldn't parse number"
      Just number -> do
        response <- liftIO $ GH.closeIssue repo number (T.unpack $ requestUsername req)
        case response of
          Right _ -> postResponse "Done!"
          Left e -> postResponse "There was an error" >> postResponse (show e)
closeIssue _ _ = postResponse "Couldn't parse that message... :("

listIssues :: (BotRequest r) => UnauthenticatedActionHandler r
listIssues [repo] _ = do
    issues <- liftIO $ GH.issues repo
    case issues of
      Left e ->  postResponse "There was an error" >> postResponse (show e)
      Right is ->
        let prettyPrintIssue i = intercalate "\n" [ "Title: " ++ GH.issueTitle i
                                                  , "Number: #" ++ show (GH.issueNumber i)
                                                  , "URL: " ++ GH.issueUrl i
                                                  ]
            text = intercalate "\n\n" $ map prettyPrintIssue $ reverse is
        in postResponse text
listIssues _ _ = postResponse "Repo name cannot contain spaces"

ruby :: (BotRequest r) => UnauthenticatedActionHandler r
ruby args _ = do
    let command = intercalate "+" args
    response <- liftIO $ evalRuby command
    case response of
      Right output -> postResponse output
      Left e -> postResponse "There was an error..." >> postResponse (show e)

requestFeature :: (BotRequest r) => UnauthenticatedActionHandler r
requestFeature = createIssue GH.SlackBotRepo

createApiIssue :: (BotRequest r) => UnauthenticatedActionHandler r
createApiIssue = createIssue GH.ApiRepo

createIssue :: BotRequest r => GH.RepoName -> UnauthenticatedActionHandler r
createIssue repo args req = do
    let pharse = intercalate "+" args
        issue = GH.FeatureRequest { title = pharse
                                  , username = T.unpack $ requestUsername req
                                  , repoName = repo
                                  }
    response <- liftIO $ GH.createIssue issue
    case response of
      Right result -> do postResponse "Noted!"
                         postResponse $ GH.issueUrl result
      Left e -> postResponse "There was an error..." >> postResponse (show e)

apiErrors :: (BotRequest r) => UnauthenticatedActionHandler r
apiErrors [from, to] _ = postNewRelicData ls NR.getErrorCount from to
    where ls = [ ("Error count", NR.errorCount) ]
apiErrors _ _ = postResponse "Parse failed"

apiMetrics :: (BotRequest r) => UnauthenticatedActionHandler r
apiMetrics [from, to] _ = postNewRelicData ls NR.getMetricsReport from to
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
apiMetrics _ _ = postResponse "Parse failed"


postNewRelicData :: [(Text, a -> Text)]
                    -> (DateRepresentation -> DateRepresentation -> IO (Either GenericException a))
                    -> String
                    -> String
                    -> StateT [ResponseRunner] IO ()
postNewRelicData ls fetchData from to = do
    void $ postResponse "1 second..."
    report <- liftIO $ runExceptT $ do
      fromDate <- ExceptT $ parseNaturalLanguageDate $ pack from
      toDate <- ExceptT $ parseNaturalLanguageDate $ pack to
      ExceptT $ fetchData fromDate toDate
    case report of
      Left e -> postResponse $ show e
      Right r -> postResponse $ intercalate "\n" $ map (\(t, f) -> T.unpack t ++ ": " ++ T.unpack (f r)) ls

tellMeAbout :: (BotRequest r) => UnauthenticatedActionHandler r
tellMeAbout args _ = do
    let phrase = intercalate "+" args
    response <- liftIO $ DG.tellMeAbout phrase
    case response of
      Left e -> postResponse (show e)
      Right answer -> postResponse answer

whatsFunctor :: (BotRequest r) => UnauthenticatedActionHandler r
whatsFunctor _ _ = postResponse $ mconcat [ "class Functor (f :: * -> *) where\n"
                                                   , "  fmap :: (a -> b) -> f a -> f b"
                                                   ]

whatsApplicative :: (BotRequest r) => UnauthenticatedActionHandler r
whatsApplicative _ _ = postResponse $ mconcat [ "class Functor f => Applicative (f :: * -> *) where\n"
                                                       , "  pure :: a -> f a\n"
                                                       , "  (<*>) :: f (a -> b) -> f a -> f b"
                                                       ]

whatsMonad :: (BotRequest r) => UnauthenticatedActionHandler r
whatsMonad _ _ = postResponse $ mconcat [ "class Applicative m => Monad (m :: * -> *) where\n"
                                                 , "  (>>=) :: m a -> (a -> m b) -> m b\n"
                                                 , "  return :: a -> m a"
                                                 ]

coffeeTime :: (BotRequest r) => UnauthenticatedActionHandler r
coffeeTime _ _ = postResponse "Let me check on that" >> postResponse "Yes it is!"

whosThere :: (BotRequest r) => AuthenticatedActionHandler r
whosThere accessToken _ _ = do
    users <- liftIO $ usersList accessToken
    case users of
      Nothing -> postResponse "Problem with slack"
      Just users' -> let x = mconcat ["Online users: ", list]
                           where list = intercalate ", " $ map slackUserName users'
                     in postResponse x

getTime :: (BotRequest r) => UnauthenticatedActionHandler r
getTime _ _ = do
    date <- liftIO $ removeExtraSpaces <$> runProcess "date"
    let text = mconcat ["The current time is ", date, ", at least where I am"]
    postResponse text

httpGet :: String -> IO String
httpGet url = BS.unpack . LBS.toStrict <$> HTTP.simpleHttp url

gif :: (BotRequest r) => UnauthenticatedActionHandler r
gif phrase _ = do
    let query = intercalate "+" phrase
    urlM <- liftIO $ gifMe query
    case urlM of
      Left e -> postResponse $ show e
      Right url -> postResponse url

-- TODO: Make a module that wraps this, and uses safeHttpLbs
cat :: (BotRequest r) => UnauthenticatedActionHandler r
cat _ _ = do
    resp <- liftIO $ httpGet "http://thecatapi.com/api/images/get?format=xml"
    case matchRegex (mkRegex "<url>(.+)</url>") resp of
      Just [url] -> postResponse url
      _ -> postResponse "Something went wrong"

pickupLunch :: (BotRequest r) => AuthenticatedActionHandler r
pickupLunch accessToken _ _ = do
    users <- liftIO $ runMaybeT $ do
      users <- MaybeT $ usersList accessToken
      MaybeT $ return <$> samples 2 users
    case users of
      Nothing -> postResponse "Problem talking to slack"
      Just users' -> let list = intercalate " and " $ map slackUserName users'
                         x = mconcat ["How about? ", list]
                     in postResponse x

timer :: (BotRequest r) => UnauthenticatedActionHandler r
timer [time] _ = do
    let
      t :: Maybe Int
      t = readMaybe time
    case t of
      Nothing -> postResponse "Couldn't parse time"
      Just t' -> do
        _ <- postResponse "Starting timer!"
        insertIO $ threadDelay $ t' * 60 * 1000000
        postResponse "Times up!"
timer _ _ = postResponse "Couldn't parse time"

flipCoin :: (BotRequest r) => UnauthenticatedActionHandler r
flipCoin _ _ = do
    result <- fromJust <$> liftIO (sample ["heads", "tails"])
    postResponse $ "Its " ++ result ++ "!"

help :: forall r . (BotRequest r) => UnauthenticatedActionHandler r
help _ _ = postResponse $ mconcat doc
  where
    categories :: [String]
    categories = map categoryParagraph $ groupBy category actions

    categoryParagraph :: (ActionCategory, [BotAction r]) -> String
    categoryParagraph (c, as) = intercalate "\n" [ "*" ++ show c ++ ":*"
                                                 , intercalate "\n" $ sort $ map command as
                                                 ]

    doc :: [String]
    doc = [ "Here are the commands I know about: "
          , "\n"
          , intercalate "\n\n" categories
          ]

randomJoke' :: (BotRequest r) => UnauthenticatedActionHandler r
randomJoke' x y =
    postResponse "Okay here is another..." >> randomJoke x y

randomJoke :: (BotRequest r) => UnauthenticatedActionHandler r
randomJoke _ _ = liftIO (sample jokes) >>= postResponse . fromJust
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

authenticateAction :: (BotRequest r) => UnauthenticatedActionHandler r
authenticateAction _ req = do
    -- TODO: Don't use environment variables here
    -- TODO: Concat the paths in a nicer way
    appRoot <- liftIO getAppRoot
    let
      (slackAuth, _) = mapFst (T.unpack . T.intercalate "/") (renderRoute SlackAuthR)
      params = [ ("team_id", requestTeamId req)
               , ("user_id", requestUserId req)
               ]
      username = requestUsername req
      authUrl = appRoot ++ slackAuth ++ T.unpack (toQueryParams params)
    liftIO $ postResponseToSlack (SlackResponseUsername username) $ T.pack authUrl
    postResponse "Check your private messages"

-- ---- Helpers

safeNth :: Int -> [a] -> Maybe a
safeNth _ [] = Nothing
safeNth 0 (x:_) = Just x
safeNth n (_:xs) = safeNth (n - 1) xs

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ safeNth idx xs

safeTake :: Int -> [a] -> [a]
safeTake _ [] = []
safeTake 0 _ = []
safeTake n (x:xs) = x : safeTake (n-1) xs

runProcess :: String -> IO String
runProcess cmd = SP.readProcess cmd [] []

removeExtraSpaces :: String -> String
removeExtraSpaces = fix (L.replace "  " " ")

samples :: Eq a => Int -> [a] -> IO [a]
samples 0 _ = return []
samples n list = do
    aSample <- fromJust <$> sample list
    let list' = reject (== aSample) list
    restOfSamples <- samples (n-1) list'
    return $ aSample : restOfSamples

reject :: (a -> Bool) -> [a] -> [a]
reject f = filter (not . f)
