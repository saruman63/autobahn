module Common (module Export, exitWithError, exitWithSuccess) where

import Control.Monad as Export
import Data.Monoid as Export
import Data.Text as Export (Text)
import System.Exit

exitWithError msg = do
    putStrLn msg
    exitWith $ ExitFailure 1

exitWithSuccess msg = do
    putStrLn msg
    exitWith ExitSuccess
