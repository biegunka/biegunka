{-# LANGUAGE TemplateHaskell #-}
-- | Making life easier with meta-programming
module Control.Biegunka.QQ
  ( sh
  , shell
  , multiline
  ) where

import Data.String (fromString)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.Command.QQ (sh, shell)


-- | 'QuasiQuoter' for raw multiline strings
multiline :: QuasiQuoter
multiline = defaultQuasiQuoter
  { quoteExp  = (\string -> [|fromString string|]) . filter (/= '\r')
  }

-- | Failing 'QuasiQuoter'
defaultQuasiQuoter :: QuasiQuoter
defaultQuasiQuoter = QuasiQuoter
  { quoteExp  = failure "expressions"
  , quotePat  = failure "patterns"
  , quoteType = failure "types"
  , quoteDec  = failure "declaration"
  }
 where
  failure kind = fail $ "multiline string quasiquoter does not support splicing " ++ kind
