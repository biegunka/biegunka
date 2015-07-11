-- | Generally useful 'Sources' related definitions
module Control.Biegunka.Source
  ( Url
  , url
  ) where

import Control.Lens

import Control.Biegunka.Language (HasOrigin(..))
import Control.Biegunka.Script (Url)


url :: HasOrigin s t a b => b -> s -> t
url = set origin
