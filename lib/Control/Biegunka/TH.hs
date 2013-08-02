{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Making life easier with meta-programming
module Control.Biegunka.TH (makeOptionsParser) where

import Data.Char (toLower)
import Data.Foldable (asum)

import Language.Haskell.TH
import Options.Applicative

import Control.Biegunka.Settings (Settings, biegunka, confirm)
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(Sources))
import Control.Biegunka.Script (Script)
import Control.Biegunka.Verify (check)


-- | Make parser for biegunka and environment options
--
-- The following options become available:
--
--   * --safe-run (default)
--
--   * --run
--
--   * --dry-run
--
--   * --check
--
--   * --full
--
--   * Also one option for each environment (the --lowercased environment name)
--
-- The usage is trivial:
--
-- > data Environments = X220 | T510
-- >
-- > makeOptionsParser ''Environment
-- >
-- > main :: IO ()
-- > main = do
-- >   (environment, runBiegunka) <- optionsParser
-- >   ...
makeOptionsParser :: Name -> Q [Dec]
makeOptionsParser name = do
  inf <- reify name
  case inf of
    TyConI (DataD _ tyCon _ dataCons _) ->
      let environment = ListE <$> mapM (makeEnvironmentFlag . conToName) dataCons in [d|
        optionsParser :: IO ($(conT tyCon), (Settings () -> Settings ()) -> Script Sources () -> IO ())
        optionsParser = customExecParser (prefs showHelpOnError) opts
         where
          opts = info (helper <*> ((,) <$> asum $(environment) <*> interpreters)) fullDesc

          interpreters =
            let safeRun = confirm <> run
            in (\i cs -> biegunka cs i) <$> asum
              [ flag' run (long "run" <>
                  help ("Do real run"))
              , flag' safeRun (long "safe-run" <>
                  help ("Do real run (after confirmation)"))
              , flag' (dryRun <> safeRun <> check) (long "full" <>
                  help ("Do dry run, real run (after confirmation) and then check results"))
              , flag' dryRun (long "dry-run" <>
                  help ("Do only dry run, do not touch anything"))
              , flag' check (long "check" <>
                  help ("Compare current filesystem state against script"))
              , pure safeRun
              ]
        |]
    _ -> fail "makeOptionsParser: Unsupported data type"


makeEnvironmentFlag :: Name -> Q Exp
makeEnvironmentFlag name = case nameBase name of
  (toLower -> b):ase ->
    let longOptionName = b:ase in [e|
       flag' $(return (ConE name)) (long longOptionName <> help ("Use " ++ longOptionName ++ " settings"))
     |]
  _ -> fail "makeFlag: Anonymous data constructor (???)"


conToName :: Con -> Name
conToName con = case con of
  NormalC n _   -> n
  RecC n _      -> n
  InfixC _ n _  -> n
  ForallC _ _ c -> conToName c
