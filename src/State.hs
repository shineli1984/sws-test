module State where

import Data.Cache (Cache)
import Web.API.Lang (CompaniesResponse, ExchangesResponse)

data State = State
  { cache :: (Cache String CompaniesResponse)
  , exchangesCache :: (Cache () ExchangesResponse)
  , accessControlAllowOrigin :: String
  , connStr :: String
  , connPoolSize :: Int
  }