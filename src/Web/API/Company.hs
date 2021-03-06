{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures, DuplicateRecordFields, OverloadedStrings, NamedFieldPuns, StandaloneDeriving, FlexibleInstances #-}

module Web.API.Company where

import Data.Proxy
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text, pack)
import Servant.API
import Servant.Server (Handler, ServerT)
import Database.Persist
import Persistence.Queries (SymbolFilter(..), ScoreFilter(..), Sorting(..), SortingDirection, run, companiesQuery, pricesQuery)
import qualified Persistence.Queries as Q
import Control.Monad.IO.Class (liftIO)
import qualified Database.Esqueleto as E
import GHC.Int (Int64)
import qualified Data.Cache as C
import Control.Monad.Trans.Reader  (ReaderT, ask)
import State (State(..))
import Web.API.Lang

type API = CompanyAPI

type CompanyAPI = "companies" :>
  ( QueryParam "numberOfPrices" Int64
    :> QueryParam "sortBy" CompanySortBy
    :> QueryFlag "desc"
    :> QueryParams "exchanges" Text
    -- TODO: possibly can use newtype wrapping for the following int64
    :> QueryParam "score" Int64
    :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] CompaniesResponse)
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

api :: Proxy API
api = Proxy

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
            companyResponses 
              = [ CompanyResponse company score close (sqrt <$> variance) (fromMaybe [] . Map.lookup (entityKey company) $ prices')
              | (company, E.Value variance, E.Value close, E.Value score) <- companies'
              ]

        pure . CompaniesResponse $ companyResponses
        ) >>= \r -> C.insert' cache Nothing cacheKey r *> pure r
