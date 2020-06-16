{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures, DuplicateRecordFields, OverloadedStrings, NamedFieldPuns, StandaloneDeriving, FlexibleInstances #-}

module Web.Api where

import Data.Proxy
import Servant.API
import Web.API.Company (CompanyAPI)
import Web.API.Exchange (ExchangeAPI)

type API = CompanyAPI :<|> ExchangeAPI

api :: Proxy API
api = Proxy
