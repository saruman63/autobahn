module Options where

import System.Environment

import Common

data Options = Options {
    optionsTest :: Bool
  }

parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    case args of
        ["-t"] -> 
            return $ Options True
        [] -> 
            return $ Options False
        _ -> 
            exitWithError "Invalid arguments.\nUsage: autobahn [-t]"
