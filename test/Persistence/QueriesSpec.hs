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
  describe "ToRawSql instances" $ do
    context "sorting direction" $ do
      it "produces sql direction" $ property $
        \(direction :: SortingDirection) ->
          sql direction `shouldBe` (toUpper . pack . show $ direction)

    context "Sorting Score" $ do
      it "produces 'order by score'" $ property $
        \(direction :: SortingDirection) ->
          sql (Score direction) `shouldBe` ("order by score " <> sql direction)

    context "filter" $ do
      it "turns Symbol into condition" $ property $
        \(sym :: String) ->
          sql (SymbolFilter . pack $ sym) `shouldBe` "swsCompany.exchange_symbol = ?"
      it "turns Score into condition" $ property $
        \(score :: Int64) ->
          sql (ScoreFilter score) `shouldBe` "score >= ?"

      it "turns both Symbol & Score filters into condition" $ property $
        \(sym :: [String], score :: Int64) -> do
          let actual = sql (SymbolFilter . pack <$> sym, Just . ScoreFilter $ score) 
          case sym of
            [] -> actual `shouldBe` "where score >= ?"
            syms -> actual `shouldBe` "where (" <> intercalate " OR " ((\_sym -> ("swsCompany.exchange_symbol = ?")) <$> syms) <> ") AND score >= ?"
              
  
  describe "ToParam instances" $ do
    context "filtering" $ do
      it "turns Symbol into list of PersistText" $ property $
        \(sym :: String) ->
          param (SymbolFilter . pack $ sym) `shouldBe` [PersistText . pack $ sym]
      it "turns Score into list of PersistInt64" $ property $
        \(score :: Int64) ->
          param (ScoreFilter score) `shouldBe` [PersistInt64 score]
