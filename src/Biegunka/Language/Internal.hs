-- | Specifies internal language primitives
module Biegunka.Language.Internal where

import Biegunka.Language.External


data IL =
    IA A Int String String
  | IS FilePath String (IO ()) Int String String
  | IW W
