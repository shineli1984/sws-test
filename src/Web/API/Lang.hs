{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, DataKinds #-}
module Web.API.Lang where

import Data.Aeson
import GHC.Generics
import Servant.API
import Database.Persist
import Persistence.Schema (SwsCompany, SwsCompanyId, SwsCompanyScore, SwsCompanyPriceClose(..))
import Persistence.Queries (Exchange(..))
import GHC.Int

data CompanyResponse 
  = CompanyResponse
  { company :: Entity SwsCompany
  , score :: Maybe Int64
  , close :: Maybe Double
  , stdd :: Maybe Double
  , closes :: [Entity SwsCompanyPriceClose]
  }
  deriving Generic
instance ToJSON CompanyResponse

data CompaniesResponse 
  = CompaniesResponse 
  { companies :: [CompanyResponse]
  }
  deriving Generic
instance ToJSON CompaniesResponse

deriving instance Generic Exchange
instance ToJSON Exchange
newtype ExchangesResponse 
  = ExchangesResponse [Exchange]
  deriving Generic
instance ToJSON ExchangesResponse

addAllowOriginHeader :: String -> a -> Headers '[Header "Access-Control-Allow-Origin" String] a
addAllowOriginHeader = addHeader