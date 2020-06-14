{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}

module Persistence.Schema where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.Aeson
import qualified Database.Esqueleto as E
import GHC.Int

share [mkPersist sqlSettings] [persistUpperCase|

SwsCompany
    Id String
    name String
    unique_symbol String
    exchange_symbol String
    score_id SwsCompanyScoreId
    deriving Show
    deriving Generic

SwsCompanyScore
    company_id SwsCompanyId
    total Int64
    date_generated String
    deriving Show
    deriving Generic

SwsCompanyPriceClose
    date Day
    company_id SwsCompanyId
    price Double
    Primary date
    deriving Show
    deriving Generic
|]

instance ToJSON  (Entity SwsCompany)
instance ToJSON  SwsCompany
deriving instance Generic (Key SwsCompany)

instance ToJSON  (Entity SwsCompanyScore)
instance ToJSON  SwsCompanyScore
deriving instance Generic (Key SwsCompanyScore)

instance ToJSON  (Entity SwsCompanyPriceClose)
instance ToJSON  SwsCompanyPriceClose
deriving instance Generic (Key SwsCompanyPriceClose)