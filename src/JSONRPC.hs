-- Simple JSON RPC over https.

module JSONRPC where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Method as HTTP

class ToJSON a => RPCMethod a where
    methodName :: a -> Text

type RPCClientT m a = ReaderT (HTTP.Request, HTTP.Manager) m a

data RPCResult a = RPCResult {
      rpcResult :: a
    }

instance FromJSON a => FromJSON (RPCResult a) where
    parseJSON (Aeson.Object o) = RPCResult 
        <$> o .: "result"
        -- <*> o .: "id" -- ignoring the ID

    parseJSON _ = fail "RPCResult is not an object."

call :: (MonadIO m, RPCMethod a, FromJSON r) => a -> RPCClientT m (Either String r)
call fn = do
    (request', manager) <- ask
    let request = makeRequest request' fn 
    res <- liftIO $ try (HTTP.httpLbs request manager)
    case res of
        Left (e :: SomeException) -> 
            return $ Left $ show e
        Right res -> 
            -- do
            -- liftIO $ putStrLn $ show $ HTTP.responseBody res
            return $ fmap rpcResult $ Aeson.eitherDecode $ HTTP.responseBody res 

    where
        makeRequest request' fn = request' {
              HTTP.method = HTTP.methodPost
            , HTTP.requestHeaders = [(HTTP.hContentType, "application/json")]
            , HTTP.requestBody = makeBody fn
            }

        makeBody fn = 
            let json = Aeson.object [
                    "method" .= methodName fn
                  , "id" .= (1 :: Int)
                  , "params" .= fn
                  ]
            in
            HTTP.RequestBodyLBS $ Aeson.encode json



