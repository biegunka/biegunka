-- | @biegunka@ tool options
module Options where

import Data.Monoid (mempty)
import Options.Applicative


-- | @biegunka@ subcommands
data BiegunkaCommand
  = Init FilePath                 -- ^ @biegunka init@
  | RunScript                     -- ^ @biegunka run@ or @biegunka check@
      FilePath [String]
  | List FilePath Format [String] -- ^ @biegunka list@
  | GenScript
      FilePath FilePath [String]  -- ^ @biegunka generate@
  | Version                       -- ^ Print @biegunka@ version
    deriving (Show, Eq)

-- | @biegunka list@ formats
data Format =
    JSON
  | Format String
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
    command "list"  (info listOptions
      (progDesc "List biegunka profiles data")) <>
    command "generate"  (info genScriptOptions
      (progDesc "Generate script for saved groups"))
   where
    listOptions = List
      <$> dataDir
      <*> listFormats
      <*> otherArguments
    listFormats =
      Format <$> strOption (long "format"
        <> value defaultBiegunkaListFormat
        <> help "Output format string")
     <|>
      flag' JSON (long "json"
        <> help "JSON Output format")

    genScriptOptions = GenScript
      <$> appDir
      <*> dataDir
      <*> otherArguments

    appDir = strOption (long "app-dir"
      <> value defaultBiegunkaAppDirectory
      <> help "Biegunka root")

    dataDir = strOption (long "data-dir"
      <> value defaultBiegunkaDataDirectory
      <> help "Biegunka data directory")

    destination = argument Just (value defaultBiegunkaScriptName)

    otherArguments = many (argument Just mempty)

-- | Filename which @biegunka init@ creates by default
defaultBiegunkaScriptName :: FilePath
defaultBiegunkaScriptName = "Biegunka.hs"

-- | @biegunka list@ default formatting options
defaultBiegunkaListFormat :: String
defaultBiegunkaListFormat = "Group %p%n%;  Source %p owned by %u%n%;    %T %p owned by %u%n"

-- | @biegunka list@ and @biegunka generate@ default data directory
defaultBiegunkaDataDirectory :: String
defaultBiegunkaDataDirectory = "~/.biegunka"

-- | @biegunka generate@ default app root
defaultBiegunkaAppDirectory :: String
defaultBiegunkaAppDirectory = "~"
