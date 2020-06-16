{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures, DuplicateRecordFields, OverloadedStrings, NamedFieldPuns, StandaloneDeriving, FlexibleInstances #-}

module Web.API.Exchange where

import Data.Aeson
import Data.Proxy
import Data.Text (Text, pack)
import Servant.API
import Servant.Server (Handler, ServerT)
import Persistence.Queries (Exchange, run, exchangesQuery)
import Control.Monad.IO.Class (liftIO)
import qualified Database.Esqueleto as E
import qualified Data.Cache as C
import Control.Monad.Trans.Reader  (ReaderT, ask)
import Web.API.Lang
import State

type API = ExchangeAPI

type ExchangeAPI = "exchanges" :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] ExchangesResponse)

api :: Proxy API
api = Proxy

exchangeServer :: ServerT ExchangeAPI (ReaderT State Handler)
exchangeServer = do
  State { exchangesCache, accessControlAllowOrigin, connStr, connPoolSize } <- ask
  let run' = run connPoolSize . pack $ connStr

  liftIO $ do
    exchangesCacheResult <- exchangesCache `C.lookup'` ()
    addAllowOriginHeader accessControlAllowOrigin <$> case exchangesCacheResult of
          Just r -> pure r
          Nothing -> 
            ( run' $ do
                exchanges' <- exchangesQuery
                let response = ExchangesResponse . fmap E.unValue $ exchanges'

                pure response
            ) >>= \r -> C.insert' exchangesCache Nothing () r *> pure r
