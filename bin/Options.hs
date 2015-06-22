-- | @biegunka@ tool options
module Options where

import           Control.Monad (guard)
import qualified Data.List as List
import           Options.Applicative


-- | @biegunka@ subcommands
data BiegunkaCommand
  = Init FilePath
  | RunScript FilePath [String]
  | Json FilePath
  | Version
    deriving (Show, Eq)


-- | @biegunka@ tool command line options parser
options :: ParserInfo BiegunkaCommand
options = info (helper <*> opts) fullDesc
 where
  opts = flag' Version (long "version" <> help "Print version") <|> subcommands

  subcommands = subparser $
    command "init" (info (Init <$> destination) (progDesc "Initialize biegunka script")) <>
    command "run"  (info (RunScript <$> destination <*> otherArguments)
      (progDesc "Run biegunka script")) <>
    command "json"  (info listOptions (progDesc "Print biegunka data as a JSON document"))
   where
    listOptions = Json
      <$> strOption (long "data-dir"
        <> value defaultBiegunkaDataDirectory <> help "Biegunka data directory")

    destination = argument (do x <- str; guard (not ("-" `List.isPrefixOf` x)); return x)
                           (value defaultBiegunkaScriptName)

    otherArguments = many (argument str idm)

-- | Filename which @biegunka init@ creates by default
defaultBiegunkaScriptName :: FilePath
defaultBiegunkaScriptName = "Biegunka.hs"

-- | @biegunka list@ and @biegunka generate@ default data directory
defaultBiegunkaDataDirectory :: String
defaultBiegunkaDataDirectory = "~/.biegunka"

-- | @biegunka generate@ default app root
defaultBiegunkaAppDirectory :: String
defaultBiegunkaAppDirectory = "~"
