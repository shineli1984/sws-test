{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Persistence.QueriesSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Persistence.Queries
import Data.Text (Text, toUpper, pack)
import GHC.Int (Int64)
import Database.Persist (PersistValue(..))
import Data.Text (intercalate)

instance Arbitrary SortingDirection where
  arbitrary = oneof [pure Desc, pure Asc]

spec :: Spec
spec = do
    pure ()
 