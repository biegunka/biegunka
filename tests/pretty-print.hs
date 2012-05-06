import Biegunka.DryRun
import Control.Applicative ((<$>))

main ∷ IO ()
main = pretty <$> load >>= putStrLn
