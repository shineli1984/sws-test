{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, PartialTypeSignatures, FlexibleContexts, NamedFieldPuns #-}

module Main where

import Web.Api (companyServer, api, Response, State(State))
import Servant.Server (serve, hoistServer)
import Network.Wai.Handler.Warp (run)
import Data.Cache (Cache, newCache)
import Control.Monad.Trans.Reader  (runReaderT)
import Options.OptionParser (readOptions, CommandLineOptions(..))
import Options.Applicative (execParser, info)

main :: IO ()
main = do
  -- setup caching
  c <- newCache Nothing :: IO (Cache String [Response])
  -- parse command line options
  CommandLineOptions { port
                     , accessControlAllowOrigin
                     , connStr
                     , connPoolSize
                     } <- execParser readOptions
  -- setup env
  let env = State c accessControlAllowOrigin connStr connPoolSize
  -- start server
  putStrLn $ "Server running at port " <> show port
  run port
    . serve api 
    . hoistServer api (`runReaderT` env)
    $ companyServer