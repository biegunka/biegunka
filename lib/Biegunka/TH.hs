{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Making life easier with meta-programming
module Biegunka.TH (makeOptionsParser) where

import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Traversable (sequenceA)
import Data.Monoid (mconcat, mempty)

import Language.Haskell.TH
import Options.Applicative

import Biegunka.Control (Controls, biegunka, confirm)
import Biegunka.Execute (run)
import Biegunka.Execute.Control (EE)
import Biegunka.Language (Scope(Profiles))
import Biegunka.Pretend (dryRun)
import Biegunka.Script (Script)
import Biegunka.Verify (check)


-- | Make parser for biegunka and environment options
--
-- The following options become available:
--
--   * --run
--
--   * --safe-run
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
        optionsParser :: IO ($(conT tyCon), (Controls -> Controls) -> (EE () -> EE ()) -> Script Profiles () -> IO ())
        optionsParser = customExecParser (prefs showHelpOnError) opts
         where
          opts = info (helper <*> ((,) <$> asum $(environment) <*> interpreters)) fullDesc

          interpreters = (\i cs -> biegunka cs . i) . mconcat <$> sequenceA
            [ flag mempty run (long "run" <>
                help ("Do real run"))
            , flag mempty (const confirm <> run) (long "safe-run" <>
                help ("Do real run (after confirmation)"))
            , flag mempty (const dryRun <> const confirm <> run <> const check) (long "full" <>
                help ("Do dry run, real run (after confirmation) and then check results"))
            , flag mempty (const dryRun) (long "dry-run" <>
                help ("Do only dry run, do not touch anything"))
            , flag mempty (const check) (long "check" <>
                help ("Compare current filesystem state against script"))
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
