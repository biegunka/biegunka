{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Making life easier with meta-programming
module Control.Biegunka.TH
  ( module Control.Biegunka.TH
  , module System.Shell.QQ
  ) where

import Data.Char
import Data.Foldable (asum)
import Language.Haskell.TH
import Options.Applicative
import System.Shell.QQ (sh, shell)

import Control.Biegunka.Biegunka (Interpreter, biegunka, confirm)
import Control.Biegunka.Settings (Settings)
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(Sources))
import Control.Biegunka.Script (Script)
import Control.Biegunka.Check (check)


-- | Make command line parser for biegunka library
-- and script environments options
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
-- Supports only "simple" sum types. ("Simple" here means non-empty, non-parametrized.)
--
-- The usage is trivial:
--
-- > data Environments = X220 | T510
-- >
-- > biegunkaOptions ''Environments
-- >
-- > main :: IO ()
-- > main = do
-- >   (environment, runBiegunka) <- options
-- >   case environment of
-- >     X220 -> runBiegunka ...
-- >     T510 -> runBiegunka ...
biegunkaOptions :: Name -> Q [Dec]
biegunkaOptions name = do
  inf <- reify name
  case inf of
    TyConI (DataD _ tyCon _ dataCons _) ->
      let environment = ListE <$> mapM (makeEnvironmentFlag . conToName) dataCons in [d|
        options :: IO ($(conT tyCon), (Settings () -> Settings ()) -> Script Sources () -> IO ())
        options = flip biegunka .: customExecParser (prefs showHelpOnError) __opts__

        __opts__ :: ParserInfo ($(conT tyCon), Interpreter)
        __opts__ = info (helper <*> ((,) <$> asum $(environment) <*> interpreters)) fullDesc
         where
          interpreters = asum
            [ flag' run (long "run" <>
                help ("Run script"))
            , flag' safeRun (long "safe-run" <>
                help ("Run script after confirmation"))
            , flag' (dryRun <> safeRun <> check) (long "full" <>
                help ("Dry run, run after confirmation and then check results"))
            , flag' dryRun (long "dry-run" <>
                help ("Dry run"))
            , flag' check (long "check" <>
                help ("Check script"))
            , pure safeRun
            ]
           where
            safeRun = confirm <> run
        |]
    _ -> fail "biegunkaOptions: Unsupported data type"

makeEnvironmentFlag :: Name -> Q Exp
makeEnvironmentFlag name =
  let longOption = transformString (nameBase name) in [e|
     flag' $(return (ConE name)) (long longOption <> help ("Use " ++ longOption ++ " settings"))
   |]

conToName :: Con -> Name
conToName con = case con of
  NormalC n _   -> n
  RecC n _      -> n
  InfixC _ n _  -> n
  ForallC _ _ c -> conToName c

-- | Transform data constructor name into command line option
--
-- >>> transformString ""
-- ""
--
-- >>> transformString "foo"
-- "foo"
--
-- >>> transformString "Foo"
-- "foo"
--
-- >>> transformString "FooBarBaz"
-- "foo-bar-baz"
transformString :: String -> String
transformString (x:xs) = toLower x : concatMap transformChar xs
 where
  transformChar y
    | isUpper y = ['-', toLower y]
    | otherwise = [y]
transformString [] = []

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap
