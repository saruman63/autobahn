module Main where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON(..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.Binary as Conduit
import qualified Data.Conduit.Network as Network
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as YAML
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified System.Directory as Directory
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.Process as Process

import BTN
import Common
import Config
import qualified JSONRPC as RPC
import Options

main :: IO ()
main = do
    -- Load options.
    (Options testMode) <- parseOptions

    -- Load configuration file.
    cfg <- loadConfig
    request <- HTTP.parseRequest $ configUrl cfg
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    let api = configAPIKey cfg

    -- Load tracking JSON file.
    tracking <- loadTrackingFile

    -- If test mode, output success and exit.
    if testMode then
        putStrLn "Would download:"
    else
        putStrLn "Downloading:"

    -- For each show, request the information for each episode.
    (tracking, torrents) <- flip runReaderT (request, manager) $ do
        mapAndUnzipM (checkShow api) tracking

    -- Download torrents.
    mapM_ (downloadTorrent testMode cfg) $ join torrents

    -- Update latest downloaded in tracking JSON file.
    when (not testMode) $
        saveTrackingFile tracking

    where
        downloadTorrent testMode cfg torrent = do
            let link = torrentLink torrent
            let name = torrentReleaseName torrent

            putStrLn $ Text.unpack name

            when (not testMode) $ do
                -- Get temporary file.
                target <- fmap (flip FilePath.addExtension "torrent") $
                        fmap (</> Text.unpack name) Directory.getTemporaryDirectory

                -- Download torrent file.
                req <- HTTP.parseRequest link

                res <- try $ runResourceT $ HTTP.httpSink req $ \_ -> Conduit.sinkFile target
                -- putStrLn target
                case res of
                    Left (e :: SomeException) ->
                        exitWithError $ show e
                    Right () ->
                        return ()



                -- Open it in transmission. 
                openTransmission target cfg

                -- Delete it.
                Directory.removePathForcibly target

        openTransmission target cfg = do
            let auth = configTransmissionUser cfg <> ":" <> configTransmissionPass cfg
            (_, _, _, p) <- Process.createProcess $ Process.proc "transmission-remote" ["-a", target, "-n", auth]
            exitCode <- Process.waitForProcess p
            when ( exitCode /= ExitSuccess) $ 
                exitWithError $ "Could not open torrent file: " <> target

            return ()

        loadTrackingFile = do
            location <- fmap (</> ".autobahn/tracking.yaml") Directory.getHomeDirectory
            res <- YAML.decodeFile location
            case res of
                Nothing ->
                    exitWithError "Could not load tracking file: ~/.autobahn/tracking.yaml"
                Just r ->
                    return $ episodes r

        saveTrackingFile tracking = do
            location <- fmap (</> ".autobahn/tracking.yaml") Directory.getHomeDirectory
            YAML.encodeFile location $ Episodes tracking

        checkShow api currentEpisode@EpisodeQuery{..} = do
            res <- RPC.call $ GetTorrents api queryName -- queryResolution queryOrigin querySource
            case res of
                Left err -> do
                    liftIO $ exitWithError err
                Right (Torrents torrents') -> do
                    let currentSeason = querySeason
                    let currentEp = queryEpisode
                    let torrents = List.filter (\Torrent{..} ->
                            -- Only take new episodes.
                            (torrentSeason, torrentEpisode) >= (currentSeason, currentEp)
                            -- Filter other parameters.
                            && "Episode" == torrentCategory
                            && queryResolution == torrentResolution
                            && queryOrigin == torrentOrigin
                            && maybeEqual querySource torrentSource
                            -- Filter dolby vision torrents.
                            && not (Text.isInfixOf ".DV." torrentReleaseName)
                          ) torrents'

                    -- Update latest episodes.
                    let (latestSeason, latestEp) = List.foldr (\Torrent{..} acc ->
                            max (torrentSeason, torrentEpisode + 1) acc
                          ) (currentSeason, currentEp) torrents
                    let latestEpisode = currentEpisode {querySeason = latestSeason, queryEpisode = latestEp}

                    return ( latestEpisode, torrents)
 
        maybeEqual Nothing _ = True
        maybeEqual (Just v) u = u == v

{-
        getTorrentLinks api torrent = do
            res <- RPC.call $ GetTorrentLink api $ torrentId torrent
            case res of
                Left err -> do
                    liftIO $ exitWithError err
                Right link -> do
                    liftIO $ putStrLn $ show (link :: Aeson.Value)
 -}                   

