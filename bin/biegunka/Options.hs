-- | @biegunka@ tool options
module Options where

import Data.Foldable (asum)
import Data.Monoid (mempty)
import Options.Applicative


-- | @biegunka@ subcommands
data BiegunkaCommand
  = Init FilePath -- ^ @biegunka init@
  | Script        -- ^ @biegunka run@ or @biegunka check@
      FilePath Script [String]
  | List FilePath String [String] -- ^ @biegunka list@

-- | Disambiguate between @biegunka run@ and @biegunka check@
data Script = Run Run | Check

-- | @biegunka run@ mode
data Run = Dry | Safe | Force | Full


-- | @biegunka@ tool command line options parser
opts :: ParserInfo BiegunkaCommand
opts = info (helper <*> subcommands) fullDesc
 where
  subcommands = subparser $
    command "init" (info (Init <$> destination) (progDesc "Initialize biegunka script")) <>
    command "run"  (info (Script <$> destination <*> (Run <$> runVariant) <*> otherArguments)
      (progDesc "Run biegunka script")) <>
    command "check"  (info (Script <$> destination <*> pure Check <*> otherArguments)
      (progDesc "Check biegunka script")) <>
    command "list"  (info listOptions
      (progDesc "List biegunka profiles data"))
   where
    runVariant = asum
      [ flag' Dry   (long "dry"   <> help "Only display a forecast and stats")
      , flag' Force (long "force" <> help "Skip confirmation")
      , flag' Full  (long "full"  <> help "Composition of run --dry, run --safe and check")
      , flag' Safe  (long "safe"  <> help "Run with confirmation [default]")
      , pure Safe
      ]

    listOptions = List
      <$> strOption (long "data-dir" <> value "~/.biegunka"       <> help "Biegunka data directory")
      <*> strOption (long "format"
        <> value defaultBiegunkaListFormat
        <> help "Output format")
      <*> otherArguments

    destination = argument Just (value defaultBiegunkaScriptName)

    otherArguments = arguments Just mempty

-- | Filename which @biegunka init@ creates by default
defaultBiegunkaScriptName :: FilePath
defaultBiegunkaScriptName = "Biegunka.hs"

-- | @biegunka list@ default formatting options
defaultBiegunkaListFormat :: String
defaultBiegunkaListFormat = "Profile %n%;  Source %p%;    %T %p"

-- | Convert from @biegunka@ tool option to biegunka script
-- autogenerated options (see "Biegunka.TH")
toScriptOption :: Script -> String
toScriptOption opt = "--" ++ case opt of
  Run Force -> "run"
  Run Safe  -> "safe-run"
  Run Dry   -> "dry-run"
  Run Full  -> "full"
  Check     -> "check"
