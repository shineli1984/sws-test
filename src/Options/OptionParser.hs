module Options.OptionParser where

import Options.Applicative
import Data.Semigroup ((<>))

data CommandLineOptions 
  = CommandLineOptions
  { connStr :: String
  , connPoolSize :: Int
  , accessControlAllowOrigin :: String 
  , port :: Int 
  }

commandLineOptinosParser :: Parser CommandLineOptions
commandLineOptinosParser 
  = CommandLineOptions
  <$> strOption
      ( long "connStr"
      <> short 'c'
      <> metavar "STRING"
      <> value "db/sws.sqlite3"
      <> showDefault
      <> help "Database connection string" )
  <*> option auto
      ( long "connPoolSize"
      <> short 's'
      <> help "Connection pool size"
      <> showDefault
      <> value 10
      <> metavar "INT" )
  <*> strOption
      ( long "allowOrigin"
      <> short 'o'
      <> metavar "STRING"
      <> help "Access-Control-Allow-Origin header" 
      <> showDefault 
      <> value "http://localhost:3000")
  <*> option auto
      ( long "port"
      <> short 'p'
      <> help "Port for server"
      <> showDefault
      <> value 9998
      <> metavar "INT" )

readOptions :: ParserInfo CommandLineOptions
readOptions 
  = info (commandLineOptinosParser <**> helper)
  ( fullDesc
  <> progDesc "Sws test"
  <> header "Companies page backend" )