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
import Data.Maybe (maybeToList)

-- TODO: make configuration as command line opts
connString :: Text
connString = "db/sws.sqlite3" 
connPoolSize :: Int
connPoolSize = 10

run :: MonadUnliftIO m => ReaderT SqlBackend (ResourceT (LoggingT m)) a -> m a
run query = runStdoutLoggingT . runResourceT $ withSqlitePool connString connPoolSize (\backend ->
    flip runSqlPool backend $ query
  )

type Q = ReaderT SqlBackend (ResourceT (LoggingT IO))

class ToRawSql a where
  sql :: a -> Text
class ToParam a where
  param :: a -> [PersistValue]
instance ToParam a => ToParam [a] where
  param ps = ps >>= param
instance ToParam a => ToParam (Maybe a) where
    param ps = param . maybeToList $ ps

data SortingDirection = Desc | Asc deriving (Show)
instance ToRawSql SortingDirection where
  sql Desc = "DESC"
  sql Asc = "ASC"

data Sorting a = Score a | Variance a deriving (Show)
instance (ToRawSql a) => ToRawSql (Sorting a) where
  sql (Score a) = "order by score " <> sql a
  sql (Variance a) = "order by variance " <> sql a

instance ToRawSql a => ToRawSql (Maybe a) where
  sql (Just sorting) = sql sorting
  sql _ = ""

newtype SymbolFilter = SymbolFilter Text
instance ToRawSql SymbolFilter where
  sql (SymbolFilter sym) = "swsCompany.exchange_symbol = ?"
instance ToParam SymbolFilter where
  param (SymbolFilter sym) = [PersistText sym]

newtype ScoreFilter = ScoreFilter Int64
instance ToRawSql ScoreFilter where
  sql (ScoreFilter score) = "score >= ?"
instance ToParam  ScoreFilter where
  param (ScoreFilter score) = [PersistInt64 score]

instance ToRawSql [SymbolFilter] where
  sql [] = ""
  sql filters = intercalate " OR " . fmap sql $ filters

instance ToRawSql ([SymbolFilter], Maybe ScoreFilter) where
  sql ([], Nothing) = ""
  sql (symbolFilters, Nothing) = "where " <> sql symbolFilters
  sql ([], Just scoreFilter) = "where " <> sql scoreFilter
  sql (symbolFilters, Just scoreFilter) = "where (" <> sql symbolFilters <> ") AND " <> sql scoreFilter

companiesQuery :: [SymbolFilter] 
               -> Maybe ScoreFilter 
               -> Maybe (Sorting SortingDirection)
               -> Q [(Entity SwsCompany, Single (Maybe Double), Single (Maybe Double), Single (Maybe Int64))]
companiesQuery symbolFilters scoreFilter sorting = 
    rawSql ( "SELECT ??,"
          <> " (SELECT AVG(t.price*t.price)-AVG(t.price)*AVG(t.price) FROM (SELECT swsCompanyPriceClose.price FROM swsCompanyPriceClose WHERE swsCompany.id=swsCompanyPriceClose.company_id ORDER BY swsCompanyPriceClose.date DESC LIMIT 90) AS t) AS variance,"
          <> " (SELECT swsCompanyPriceClose.price FROM swsCompanyPriceClose WHERE swsCompany.id = swsCompanyPriceClose.company_id ORDER BY swsCompanyPriceClose.date DESC LIMIT 1),"
          <> " (SELECT swsCompanyScore.total FROM swsCompanyScore WHERE swsCompany.id=swsCompanyScore.company_id ORDER BY swsCompanyScore.date_generated DESC LIMIT 1) AS score"
          <> " FROM swsCompany"
          -- where
          <> " " <> sql (symbolFilters, scoreFilter)
          -- order by
          <> " " <> sql sorting
           ) $ param symbolFilters <> param scoreFilter

-- Because of a potential bug https://github.com/bitemyapp/esqueleto/issues/188, the following should but does not work.
-- companiesQuery filters scoreFilter sorting =
--   select $ do
--       t2 <- from $ Table @SwsCompany

--       variance <- pure $ subSelectMaybe $ do
--         t1 <- from $ SelectQuery $ do
--             t <- from $ Table @SwsCompanyPriceClose
--             orderBy [desc $ t ^. SwsCompanyPriceCloseDate]
--             limit 90
--             pure t
--         where_ $ t1 ^. SwsCompanyPriceCloseCompany_id ==. t2 ^. SwsCompanyId
--         pure $ avg_ (t1 ^. SwsCompanyPriceClosePrice)

--       lastPrice <- pure $ subSelect $ do
--         t <- from $ SelectQuery $ do
--             t <- from $ Table @SwsCompanyPriceClose
--             orderBy [desc $ t ^. SwsCompanyPriceCloseDate]
--             limit 1
--             where_ $ t ^. SwsCompanyPriceCloseCompany_id ==. t2 ^. SwsCompanyId
--             pure t
--         pure $ t ^. SwsCompanyPriceClosePrice

--       overallScore <- pure $ subSelect $ do 
--         t <- from $ SelectQuery $ do
--           t <- from $ Table @SwsCompanyScore
--           orderBy [desc $ t ^. SwsCompanyScoreDate_generated]
--           limit 1
--           where_ $ t ^. SwsCompanyScoreCompany_id ==. t2 ^. SwsCompanyId
--           pure t
--         pure $ t ^. SwsCompanyScoreTotal

--       where_ . ( foldr
--                ( \(SymbolFilter cur) acc -> 
--                   acc ||. (t2 ^. SwsCompanyUnique_symbol ==. (val . unpack $ cur)))
--                ( val (1 :: Int64) ==. val 1) )
--              $ filters

--       case scoreFilter of
--         Just (ScoreFilter score) ->
--           where_ $ overallScore >=. (just . val $ score)
--         Nothing -> pure ()

--       case sorting of
--         Just (Score Asc) -> orderBy [asc $ overallScore]
--         Just (Score Desc) -> orderBy [desc $ overallScore]
--         Just (Variance Asc) -> orderBy [asc variance]
--         Just (Variance Desc) -> orderBy [desc variance]
--         Nothing -> pure ()

--       pure (t2, variance, lastPrice, overallScore)

pricesQuery :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend, Functor f) => Int64 -> f (Key SwsCompany) -> f (ReaderT backend m [Entity SwsCompanyPriceClose])
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


-- -- -- for ad-hoc ghci test
-- test :: IO ()
-- test = do
--   companies <- run . companiesQuery [] Nothing $ Nothing
--   print companies
--   -- let companyIds :: [Key SwsCompany] = (\(company, _, _, _) -> entityKey company) <$> companies
--   -- prices <- traverse run $ pricesQuery 90 companyIds
--   pure ()