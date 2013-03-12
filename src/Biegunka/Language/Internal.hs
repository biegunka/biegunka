-- | Specifies internal language primitives
module Biegunka.Language.Internal where

import Biegunka.Language.External


data IL =
    IA Action Int String String
  | IS FilePath String (IO ()) Int String String
  | IW Wrapper
