{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, PartialTypeSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Persistence.Queries where

import Persistence.Schema
import qualified Database.Persist as P
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Conduit
import Database.Sqlite
import Database.Persist.Sqlite (wrapConnection, withSqlitePool)
import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental
import qualified Data.Map as Map
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text hiding (filter, foldr)
import GHC.Int (Int64)
import qualified Data.List as L
import Data.Aeson (ToJSON)
import Data.Maybe (maybeToList)

run :: MonadUnliftIO m => Int -> Text -> ReaderT SqlBackend (ResourceT (LoggingT m)) a -> m a
run connPoolSize connString query = runStdoutLoggingT . runResourceT $ withSqlitePool connString connPoolSize (\backend ->
    flip runSqlPool backend $ query
  )

type Q = ReaderT SqlBackend (ResourceT (LoggingT IO))
data SortingDirection = Desc | Asc deriving (Show)
data Sorting a = Score a | Variance a deriving (Show)
newtype SymbolFilter = SymbolFilter Text
newtype ScoreFilter = ScoreFilter Int64

companiesQuery :: [SymbolFilter] 
               -> Maybe ScoreFilter 
               -> Maybe (Sorting SortingDirection)
               -> Q [(Entity SwsCompany, Value (Maybe Double), Value (Maybe Double), Value (Maybe Int64))]
companiesQuery filters scoreFilter sorting =
  select $ do
      t2 <- from $ Table @SwsCompany
      
      -- varaince
      variance <- pure $ subSelectMaybe $ do
        t1 <- from $ SelectQuery $ do
            t <- from $ Table @SwsCompanyPriceClose
            orderBy [desc $ t ^. SwsCompanyPriceCloseDate]
            limit 90
            pure t
        where_ $ t1 ^. SwsCompanyPriceCloseCompany_id ==. t2 ^. SwsCompanyId
        pure $ avg_ (t1 ^. SwsCompanyPriceClosePrice)
      
      -- last price
      lastPrice <- pure $ subSelect $ do
        t <- from $ SelectQuery $ do
            t <- from $ Table @SwsCompanyPriceClose
            orderBy [desc $ t ^. SwsCompanyPriceCloseDate]
            limit 1
            where_ $ t ^. SwsCompanyPriceCloseCompany_id ==. t2 ^. SwsCompanyId
            pure t
        pure $ t ^. SwsCompanyPriceClosePrice

      -- latest overall score
      overallScore <- pure $ subSelect $ do 
        t <- from $ SelectQuery $ do
          t <- from $ Table @SwsCompanyScore
          orderBy [desc $ t ^. SwsCompanyScoreDate_generated]
          limit 1
          where_ $ t ^. SwsCompanyScoreCompany_id ==. t2 ^. SwsCompanyId
          pure t
        pure $ t ^. SwsCompanyScoreTotal

      -- filtering by exchange symbols if they exists
      case filters of
        [] -> pure ()
        (SymbolFilter symbol) : symbols ->
          where_ . ( foldr
                    ( \(SymbolFilter cur) acc -> 
                        acc ||. (t2 ^. SwsCompanyExchange_symbol ==. (val . unpack $ cur)))
                    (t2 ^. SwsCompanyExchange_symbol ==. (val . unpack $ symbol) )
                 ) $ symbols

      -- filter above score if it passed in
      case scoreFilter of
        Just (ScoreFilter score) ->
          where_ $ overallScore >=. (just . val $ score)
        Nothing -> pure ()

      -- add soring by score or variance
      case sorting of
        Just (Score Asc) -> orderBy [asc $ overallScore]
        Just (Score Desc) -> orderBy [desc $ overallScore]
        Just (Variance Asc) -> orderBy [asc variance]
        Just (Variance Desc) -> orderBy [desc variance]
        Nothing -> pure ()

      pure (t2, variance, lastPrice, overallScore)

pricesQuery :: Int64 -> [(Key SwsCompany)] -> [Q [Entity SwsCompanyPriceClose]]
pricesQuery inPastNBars companyIds =
    getPrices <$> companyIds
  where
    getPrices companyId = query
      where
        query = select $ do 
                  close <- from $ Table @SwsCompanyPriceClose
                  orderBy [desc (close ^. SwsCompanyPriceCloseDate)]
                  where_ $ close ^. SwsCompanyPriceCloseCompany_id ==. val companyId
                  limit inPastNBars
                  pure close

newtype Exchange = Exchange String
exchangesQuery :: Q [Value Exchange]
exchangesQuery = do
  exchanges <- select . distinct $ do
      company <- from $ Table @SwsCompany
      pure $ company ^. SwsCompanyExchange_symbol
  pure $ (\e -> Exchange <$> e) <$> exchanges

