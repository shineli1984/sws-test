{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, PartialTypeSignatures, FlexibleContexts, NamedFieldPuns #-}

module Main where

import Web.API.Exchange (exchangeServer)
import Web.API.Company (companyServer)
import Web.Api (api)
import qualified State as S
import Servant.Server (serve, hoistServer)
import Servant.API ((:<|>)(..))
import Network.Wai.Handler.Warp (run)
import Data.Cache (Cache, newCache)
import Control.Monad.Trans.Reader  (runReaderT)
import Options.OptionParser (readOptions, CommandLineOptions(..))
import Options.Applicative (execParser, info)

main :: IO ()
main = do
  -- setup caching
  c <- newCache Nothing
  c' <- newCache Nothing
  -- parse command line options
  CommandLineOptions { port
                     , accessControlAllowOrigin
                     , connStr
                     , connPoolSize
                     } <- execParser readOptions
  -- setup env
  let env = S.State c c' accessControlAllowOrigin connStr connPoolSize
  -- start server
  putStrLn $ "Server running at port " <> show port
  run port
    . serve api 
    . hoistServer api (`runReaderT` env)
    $ companyServer :<|> exchangeServer