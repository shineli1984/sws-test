{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, PartialTypeSignatures, FlexibleContexts #-}

module Main where

import Web.Api (companyServer, api, Response, State(..))
import Servant.Server (serve, hoistServer)
import Network.Wai.Handler.Warp (run)
import Data.Cache (Cache, newCache)
import Control.Monad.Trans.Reader  (runReaderT)

port = 9998

main :: IO ()
main = do
  c <- newCache Nothing :: IO (Cache String [Response])
  putStrLn $ "Server running at port " <> show port
  run port
    . serve api 
    . hoistServer api (`runReaderT` State c) 
    $ companyServer