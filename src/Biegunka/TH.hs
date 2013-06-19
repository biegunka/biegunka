{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.TH (makeOptionsParser) where

import Data.Char (toLower)
import Data.Foldable (asum)

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

          interpreters = asum
            [ flag' run    (long "run"     <>
                help ("Do dry run, real run (after confirmation) and then check results"))
            , flag' dryRun (long "dry-run" <> help ("Do only dry run, do not touch anything"))
            , flag' check  (long "check"   <> help ("Compare current filesystem state against script"))
            ]
           where
            dryRun cs _  = biegunka cs pretend
            run    cs es = biegunka cs (pretend <> confirm <> execute es <> verify)
            check  cs _  = biegunka cs verify
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
