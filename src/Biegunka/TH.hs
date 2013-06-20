{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.TH (makeOptionsParser) where

import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Monoid (mempty)

import Language.Haskell.TH
import Options.Applicative

import Biegunka.Control (Controls, biegunka, confirm)
import Biegunka.Execute (execute)
import Biegunka.Execute.Control (EE)
import Biegunka.Language (Scope(Profiles))
import Biegunka.Pretend (pretend)
import Biegunka.Script (Script)
import Biegunka.Verify (verify)


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

          interpreters = (\i -> (\cs es -> biegunka cs (i es))) <$>
            foldr (liftA2 (\i a -> (\cs -> i cs <> a cs))) (pure (const mempty))
              [ flag (const mempty) execute (long "run" <>
                  help ("Do real run"))
              , flag (const mempty) (\es -> confirm <> execute es) (long "safe-run" <>
                  help ("Do real run (after confirmation)"))
              , flag (const mempty) (\es -> pretend <> confirm <> execute es <> verify) (long "--all" <>
                  help ("Do dry run, real run (after confirmation) and then check results"))
              , flag (const mempty) (const pretend) (long "dry-run" <>
                  help ("Do only dry run, do not touch anything"))
              , flag (const mempty) (const verify) (long "check" <>
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
