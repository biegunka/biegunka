{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.TH (makeOptionsParser) where

import Data.Char (toLower)

import Language.Haskell.TH
import Options.Applicative


makeOptionsParser :: Name -> Q [Dec]
makeOptionsParser name = do
  inf <- reify name
  case inf of
    TyConI (DataD _ tyCon _ dataCons _) -> do
      flags <- mapM (makeFlag . conToName) dataCons
      [d|
        optionsParser :: IO $(conT tyCon)
        optionsParser = customExecParser (prefs showHelpOnError) opts
          where
            opts = info (helper <*> foldr (<|>) empty $(return (ListE flags))) fullDesc
        |]
    _ -> fail "makeOptionsParser: Unsupported data type"


makeFlag :: Name -> Q Exp
makeFlag name = case nameBase name of
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
