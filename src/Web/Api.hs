{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures, DuplicateRecordFields, OverloadedStrings, NamedFieldPuns #-}

module Web.Api where

import Data.Aeson
import Data.Proxy
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text, pack)
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant.Server (Handler, ServerT)
import Database.Persist
import Persistence.Schema (SwsCompany, SwsCompanyId, SwsCompanyScore, SwsCompanyPriceClose(..))
import Persistence.Queries (SymbolFilter(..), ScoreFilter(..), Sorting(..), SortingDirection, run, companiesQuery, pricesQuery)
import qualified Persistence.Queries as Q
import Control.Monad.IO.Class (liftIO)
import Data.Tuple.Extra (uncurry3, fst3)
import qualified Database.Esqueleto as E
import GHC.Int (Int64)
import qualified Statistics.Sample as SS
import qualified Data.Vector as V
import Web.Internal.HttpApiData (FromHttpApiData(..))
import Data.Cache (Cache)
import qualified Data.Cache as C
import Control.Monad.Trans.Reader  (ReaderT, ask)

type API = CompanyAPI

type CompanyAPI = "companies" :>
  ( QueryParam "numberOfPrices" Int64
    :> QueryParam "sortBy" CompanySortBy
    :> QueryFlag "desc"
    :> QueryParams "exchanges" Text
    -- TODO: possibly can use newtype wrapping for the following int64
    :> QueryParam "score" Int64
    :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] [Response])
  )
data CompanySortBy
  = SortByScore 
  | SortByVariance
  deriving Show

toSortingQuery :: Maybe CompanySortBy -> Bool -> Maybe (Sorting SortingDirection)
toSortingQuery Nothing = const Nothing
toSortingQuery (Just SortByScore) = pure . Score . toSortingDirectionQuery
toSortingQuery (Just SortByVariance) = pure . Variance . toSortingDirectionQuery

toSortingDirectionQuery :: Bool -> SortingDirection
toSortingDirectionQuery True = Q.Desc
toSortingDirectionQuery _ = Q.Asc

instance FromHttpApiData CompanySortBy where
  parseQueryParam "score" = Right SortByScore
  parseQueryParam "variance" = Right SortByVariance
  parseQueryParam s = Left $ "can not parse query param: " <> s

maxNumberOfPrices :: Int64
maxNumberOfPrices = 80
maxNumberOfCompanies :: Int64
maxNumberOfCompanies = 20

data Response 
  = Response
  { company :: Entity SwsCompany
  , score :: Maybe Int64
  , close :: Maybe Double
  , stdd :: Maybe Double
  , closes :: [Entity SwsCompanyPriceClose]
  }
  deriving Generic
instance ToJSON Response

api :: Proxy API
api = Proxy

data State = State
  { cache :: (Cache String [Response])
  , accessControlAllowOrigin :: String
  , connStr :: String
  , connPoolSize :: Int
  }

addAllowOriginHeader :: String -> a -> Headers '[Header "Access-Control-Allow-Origin" String] a
addAllowOriginHeader = addHeader

companyServer :: ServerT CompanyAPI (ReaderT State Handler)
companyServer numberOfPrices sortBy desc exchanges score = do 
  State { cache, accessControlAllowOrigin, connStr, connPoolSize } <- ask

  let cacheKey = (show numberOfPrices) 
              <> (show sortBy) 
              <> (show desc) 
              <> (show exchanges) 
              <> (show score) 
      run' = run connPoolSize . pack $ connStr

  liftIO $ do
    cacheResult <- cache `C.lookup'` cacheKey
    addAllowOriginHeader accessControlAllowOrigin <$> case cacheResult of
      Just r -> pure r
      Nothing -> ( run' $ do
        let symbols = fmap SymbolFilter exchanges
            score' = fmap ScoreFilter $ score

        companies' <- companiesQuery symbols score' (toSortingQuery sortBy desc)

        let companyIds = fmap (entityKey . (\(c, _, _, _) -> c)) $ companies'

        prices <-
          case numberOfPrices of
              Just n | n > 0 -> 
                fmap (zip companyIds)
                . sequence
                . pricesQuery (min n maxNumberOfPrices)
                $ companyIds
              _ -> pure []

        let prices' = Map.fromList prices

        pure 
            $ [ Response company score close (sqrt <$> variance) (fromMaybe [] . Map.lookup (entityKey company) $ prices')
            | (company, E.Value variance, E.Value close, E.Value score) <- companies'
            ]
        ) >>= \r -> C.insert' cache Nothing cacheKey r *> pure r
