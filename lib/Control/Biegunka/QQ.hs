{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | QuasiQuoters
module Control.Biegunka.QQ
  ( sh
  , Eval(..), Embed(..)
  ) where

import           Control.Applicative ((<$), pure)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Exit (ExitCode)
import           System.Posix.Env (getEnvDefault)
import qualified System.Process as Proc


-- | QuasiQuoter for shell scripts
sh :: QuasiQuoter
sh = QuasiQuoter
  { quoteExp  = quoteShellExp
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }


-- | Different interesting return types for 'sh' QuasiQuoter
--
-- Instances mostly resemble the types of things in "System.Process"
class Eval r where
  eval :: String -> [String] -> r

-- | Most basic instance: nothing is known about what happened in shell
instance Eval (IO ()) where
  eval command args = () <$ Proc.rawSystem command args

-- | Return only exit code of shell process
instance Eval (IO ExitCode) where
  eval command args = Proc.rawSystem command args

-- | Return only stdout of shell process
instance Eval (IO String) where
  eval command args = Proc.readProcess command args ""

-- | Return exit code, stdout, and stderr of shell process
instance
  ( status ~ ExitCode
  , out    ~ String
  , err    ~ String
  ) => Eval (IO (status, out, err)) where
  eval command args = Proc.readProcessWithExitCode command args ""

-- | Return exit code, stdout, and stderr of shell process
-- and consume stdin from supplied 'String'
instance
  ( status ~ ExitCode
  , out    ~ String
  , err    ~ String
  ) => Eval (String -> IO (status, out, err)) where
  eval command args stdin = Proc.readProcessWithExitCode command args stdin


-- | Embed haskell values into shell scripts
--
-- Instances provided for all "Prelude" data types for
-- which it makes sense
--
-- I recommend using @-XExtendedDefaultRules@ for modules
-- where you want to embed values, it would save for annoying
-- type annotations for numeric literals
class Embed a where
  embed :: a -> String

instance Embed Integer where
  embed = show

instance Embed Int where
  embed = show

instance Embed Float where
  embed = show

instance Embed Double where
  embed = show

instance Embed Char where
  embed = pure

instance Embed String where
  embed = id


-- | Construct shell call
quoteShellExp :: String -> Q Exp
quoteShellExp s = do
  shellEx <- runIO $ getEnvDefault "SHELL" "/bin/sh"
  [e| eval shellEx ["-c", $(string2exp s)] |]

-- | Parse references to Haskell variables
string2exp :: String -> Q Exp
string2exp = raw where
  raw ('\\':'$':xs) = [e| '$' : $(raw xs) |]
  raw ('$':'{':xs)  = [e| $(var xs) |]
  raw (x  :xs)      = [e| x : $(raw xs) |]
  raw []            = [e| [] |]

  var xs = case break (== '}') xs of
    (name, '}':ys) -> [e| embed $(return (VarE (mkName name))) ++ $(raw ys) |]
    (name, _)      -> fail $ "Bad variable pattern: ${" ++ name
