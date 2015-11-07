module EnvHelpers
    ( loadEnvironmentVariables
    )
  where

import Import
import System.Directory
import Data.List.Split
import System.Environment

loadEnvironmentVariables :: IO ()
loadEnvironmentVariables = do
    putStrLn "loading env"
    when' (doesFileExist ".env") $ do
      contents <- readFile ".env"
      mapM_ (setEnvVar . parseLine) $ lines contents

when' :: (Monad m) => m Bool -> m () -> m ()
when' cond f = cond >>= flip when f

parseLine :: String -> (String, String)
parseLine line = case splitOn "=" line of
                   [key, value] -> (key, value)
                   _ -> error "Couldn't parse .env file"

setEnvVar :: (String, String) -> IO ()
setEnvVar (key, value) = setEnv key value
