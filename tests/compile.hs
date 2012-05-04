import Control.Monad (void)
import Biegunka.DryRun

main ∷ IO ()
main = void $ git "https://github.com/supki/utils" "/home/maksenov/sandbox/utils" --> compile
  where compile = compile_with GHC "mpd/scrobbler.hs" "bin/compile-test"
