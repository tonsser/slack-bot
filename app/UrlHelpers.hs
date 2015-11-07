module UrlHelpers
    ( toQueryParams
    , concatPaths
    , getAppRoot
    , fix
    )
  where

import Import
import qualified Data.List.Utils as L
import qualified Data.Text as T
import System.Environment

toQueryParams :: [(Text, Text)] -> Text
toQueryParams params = T.append "?" (T.intercalate "&" $ map (uncurry toParam) params)
  where x +|+ y = T.append x y
        toParam key value = key +|+ "=" +|+ value

concatPaths :: [Text] -> Text
concatPaths = fixHttpProtocol . fixHttpsProtocol . removeDuplicateSlashes . T.intercalate "/"
  where removeDuplicateSlashes = fix (T.pack . L.replace "//" "/" . T.unpack)
        fixHttpProtocol = T.pack . L.replace "http:/" "http://" . T.unpack
        fixHttpsProtocol = T.pack . L.replace "https:/" "https://" . T.unpack

getAppRoot :: IO String
getAppRoot = do
    root <- fmap (++ "/") <$> lookupEnv "APPROOT"
    return $ fromMaybe "http://localhost:3000/" root

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = let x' = f x
              in if x == x'
                   then x'
                   else fix f x'
