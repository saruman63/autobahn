module BTN where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Common
import qualified JSONRPC as RPC

-- data GetTorrentLink = GetTorrentLink Text Text
-- 
-- instance RPC.RPCMethod GetTorrentLink where
--     methodName _ = "getTorrentsUrl"
-- 
-- instance ToJSON GetTorrentLink where
--     toJSON (GetTorrentLink key torrentId) = Aeson.object [
--           "0" .= key
--         , "1" .= torrentId
--         ]

data GetTorrents = GetTorrents Text Text -- Text Text (Maybe Text)

instance RPC.RPCMethod GetTorrents where
    methodName (GetTorrents _ _) = "getTorrents"

instance ToJSON GetTorrents where
    toJSON (GetTorrents key name) = do
        let c = 100000 :: Int
        let query' = [
              --   "category" .= ("Episode" :: Text)
              -- , "resolution" .= resolution
              -- , "origin" .= origin
                "series" .= name
              ]
        -- let query = case sourceM of
        --       Nothing -> query'
        --       Just s -> ("source" .= s):query'
        Aeson.object [
              "0" .= key
            , "1" .= Aeson.object query'
            , "2" .= c
            , "3" .= (0 :: Int)
            ]
    
    -- where
        
        -- toJSON [toJSON key, toJSON ("DC's Legends of Tomorrow" :: Text), toJSON c, toJSON (0 :: Int)]
        -- toJSON [toJSON key, Aeson.object ["series" .= ("DC's Legends of Tomorrow" :: Text)], toJSON c]
        --
data EpisodeQuery = EpisodeQuery {
    queryName :: Text
  , querySeason :: Int
  , queryEpisode :: Int
  , queryResolution :: Text
  , queryOrigin :: Maybe Text
  , querySource :: Maybe Text
  }
  -- TODO: Add origin and resolution (at least) XXX
  -- Update query (also add Category)

instance FromJSON EpisodeQuery where
    parseJSON (Aeson.Object o) = EpisodeQuery
        <$> o .: "name"
        <*> o .: "season"
        <*> o .: "episode"
        <*> o .: "resolution"
        <*> o .:? "origin"
        <*> o .:? "source"
    parseJSON _ = fail "Episode is not an object"
    
instance ToJSON EpisodeQuery where
    toJSON (EpisodeQuery name season episode resolution origin source) = 
        let os = [
                "name" .= name
              , "season" .= season
              , "episode" .= episode
              , "resolution" .= resolution
              ]
        in
        Aeson.object $ optionalField "origin" origin
                     $ optionalField "source" source os

      where optionalField k Nothing xs  = xs
            optionalField k (Just v) xs = (k .= v):xs

newtype Episodes = Episodes {
    episodes :: [EpisodeQuery]
  }

instance FromJSON Episodes where
    parseJSON (Aeson.Object o) = Episodes
        <$> o .: "btn"
    parseJSON _ = fail "Episodes is not an object"

instance ToJSON Episodes where
    toJSON (Episodes e) = Aeson.object ["btn" .= e]

data Torrent = Torrent {
      torrentId :: Int
    , torrentSeason :: Int
    , torrentEpisode :: Int
    , torrentLink :: Text
    , torrentReleaseName :: Text
    , torrentCategory :: Text
    , torrentResolution :: Text
    , torrentOrigin :: Text
    , torrentSource :: Text
    , torrentSeeders :: Int
    , torrentSnatched :: Int
    , torrentLeechers :: Int
    , torrentSize :: Int
    }
    deriving (Show)
    
data Torrents = Torrents {
        torrents :: [Torrent]
    }
    deriving (Show)

instance FromJSON Torrents where
    parseJSON = Aeson.withObject "Torrents" $ \o -> do
        torrents <- Map.toList <$> o .: "torrents"
        ts <- foldM (\acc (k', v) -> Aeson.withObject "Torrent" ( \v -> do
                  k <- toInt k'
                  ep' <- v .: "GroupName"
                  link <- v .: "DownloadURL"
                  name <- v .: "ReleaseName"
                  category <- v .: "Category"
                  resolution <- v .: "Resolution"
                  origin <- v .: "Origin"
                  source <- v .: "Source"
                  seeders <- v .: "Seeders" >>= toInt
                  snatched <- v .: "Snatched" >>= toInt
                  leechers <- v .: "Leechers" >>= toInt
                  size <- v .: "Size" >>= toInt

                  -- Skips episodes where the groupname can't be parsed.
                  case parseGroupName ep' of
                      Left err ->
                          -- fail $ "Could not parse GroupName (" <> Text.unpack ep' <> "): " <> err
                          return acc
                      Right ( season, episode) -> 
                          return $ (Torrent k season episode link name category resolution origin source seeders snatched leechers size):acc
                ) v
              ) [] torrents
        return $ Torrents ts

        where
            parseGroupName = Atto.parseOnly $ do
                Atto.char 'S'
                Atto.skipWhile (== '0')
                s <- Atto.decimal
                Atto.char 'E'
                Atto.skipWhile (== '0')
                e <- Atto.decimal
                Atto.endOfInput
                return ( s, e)
            toInt i = maybe (fail $ "Invalid int " <> i) pure $ readMaybe i

