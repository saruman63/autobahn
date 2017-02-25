module Config where

import Data.Aeson (FromJSON(..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as YAML
import System.FilePath ((</>))
import qualified System.Directory as Directory

import Common

data Config = Config {
      configUrl :: String
    -- , configPort :: Int
    , configAPIKey :: Text
    , configTransmissionUser :: String
    , configTransmissionPass :: String
    }

instance FromJSON Config where
    parseJSON (Aeson.Object o) = Config 
        <$> o .: "url"
        <*> o .: "api_key"
        <*> o .: "transmission_user"
        <*> o .: "transmission_password"
    parseJSON _ = fail "Not an object."

loadConfig :: IO Config
loadConfig = do
    location <- fmap (</> ".autobahn/settings.yaml") Directory.getHomeDirectory
    res <- YAML.decodeFile location
    case res of
        Nothing ->
            exitWithError "Could not load settings file: ~/.autobahn/settings.yaml"
        Just r ->
            return r
