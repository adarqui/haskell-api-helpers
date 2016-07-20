{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-orphans #-}

module Haskell.Api.Helpers (
  SpecificApiOptions,
  defaultSpecificApiOptions,
  handleError,
  getAt,
  postAt,
  putAt,
  deleteAt
) where



import           Control.Exception          (catch)
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (asks)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             toJSON)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default
import           Data.List                  (find)
import           Data.Monoid                ((<>))
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import           Haskell.Api.Helpers.Shared
import qualified Network.Connection         as Network (TLSSettings (..))
import           Network.HTTP.Client        (HttpException (..))
import qualified Network.HTTP.Conduit       as Conduit (ManagerSettings,
                                                        mkManagerSettings)
import           Network.HTTP.Types.Status  (status500, statusMessage)
import           Network.Wreq               (Options, Response, defaults,
                                             deleteWith, getWith, header, param,
                                             postWith, putWith, responseBody,
                                             responseStatus, statusCode)
import qualified Network.Wreq.Types         as WreqTypes (Options (..), manager)



type SpecificApiOptions = WreqTypes.Options



settings :: Conduit.ManagerSettings
settings = Conduit.mkManagerSettings (Network.TLSSettingsSimple True False False) Nothing



instance Default WreqTypes.Options where
  def = defaultSpecificApiOptions

defaultSpecificApiOptions :: WreqTypes.Options
defaultSpecificApiOptions = defaults {
  WreqTypes.manager = Left settings -- Left tlsManagerSettings
}



fixOpts :: [(Text, Text)] -> ApiEff WreqTypes.Options Options
fixOpts params' = do

  mapi_key <- asks apiKey
  mapi_key_header <- asks apiKeyHeader
  options'  <- asks apiOptions

  let
    opts = case (mapi_key, mapi_key_header) of
      (Just api_key, Just api_key_header) -> options' & header api_key_header .~ [api_key]
      _                                   -> options'

    opts_with_params = Prelude.foldl (\acc (k, v) -> acc & param k .~ [v]) opts params'

  pure opts_with_params



_eitherDecode :: (FromJSON a, Default a) => ByteString -> a
_eitherDecode bs =
  case eitherDecode bs of
    Left _  -> def
    Right v -> v



handleError :: (FromJSON a, FromJSON b, Default b) => RawApiResult -> Either (ApiError b) a
handleError (Left (status, body)) = Left $ ServerError status (_eitherDecode body)
handleError (Right bs)    =
  case eitherDecode bs of
    Left err -> Left $ DecodeError $ cs err
    Right a  -> Right a



internalAction
  :: forall (m :: * -> *).
     (MonadIO m)
  => IO (Response ByteString)
  -> m RawApiResult
internalAction act = liftIO ((act >>= properResponse) `catch` handler)
  where
  handler (StatusCodeException s headers _) =
     -- This basically makes this library specific to my ln-* project.
     -- It looks for the X-jSON-ERROR header, and if it is set, returns
     -- that message. This message may then be a JSON string, which can give us
     -- more detailed error information.
     --
     case find ((==) "X-jSON-ERROR" . fst) headers of
       Nothing        -> pure $ Left (s, cs $ statusMessage s)
       Just (_, body) -> pure $ Left (s, cs body)
  handler _                                 = pure $ Left (status500, "wreq")



properResponse :: Monad m => Response ByteString -> m RawApiResult
properResponse r =
  case (r ^. responseStatus ^. statusCode) of
    200 -> pure $ Right (r ^. responseBody)
    _   -> pure $ Left ((r ^. responseStatus), (r ^. responseBody))



getAt :: (QueryParam qp)  => [qp] -> [Text] -> ApiEff WreqTypes.Options RawApiResult
getAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (apiLog ("getAt: " <> url'))
  internalAction $ getWith opts url'



postAt :: (QueryParam qp, ToJSON a) => [qp] -> [Text] -> a -> ApiEff WreqTypes.Options RawApiResult
postAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (apiLog ("postAt: " <> url'))
  internalAction $ postWith opts url' (toJSON body)



putAt :: (QueryParam qp, ToJSON a) => [qp] -> [Text] -> a -> ApiEff WreqTypes.Options RawApiResult
putAt params' paths body = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (apiLog ("putAt: " <> url'))
  internalAction $ putWith opts url' (toJSON body)



deleteAt :: QueryParam qp => [qp] -> [Text] -> ApiEff WreqTypes.Options RawApiResult
deleteAt params' paths = do

  opts <- fixOpts $ map qp params'
  url <- urlFromReader

  let url' = routeQueryBy' url paths params'
  runDebug (apiLog ("deleteAt: " <> url'))
  internalAction $ deleteWith opts url'
