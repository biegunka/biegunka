module Main where

import Control.Category ((.))
import Prelude hiding ((.))
import Options.Applicative

import qualified Biegunka as B


main ∷ IO ()
main = execParser parserInfo >>= doBiegunka
 where
  parserInfo = info (helper <*> parser) $
    fullDesc . progDesc "Console line interface for Biegunka library." . header "biegunka"


data Options = Options
  { wipe ∷ Bool
  , remove ∷ FilePath
  , file ∷ FilePath
  } deriving Show


parser ∷ Parser Options
parser = Options
  <$> switch (long "wipe" . help "Remove everything Biegunka has installed." . value False)
  <*> strOption (long "remove" . metavar "REPO" . help "Remove from repository REPO" . value "")
  <*> strOption (long "file" . metavar "FILE" . help "Remove only this FILE from specified REPO" . value "")


doBiegunka ∷ Options → IO ()
doBiegunka Options { wipe = True } = B.withBiegunka B.wipe
doBiegunka Options { remove = r@(_:_), file = [] } = B.withBiegunka (B.purge r)
doBiegunka Options { remove = r@(_:_), file = f } = B.withBiegunka (B.delete r f)
doBiegunka _ = putStrLn "You should specify either a REPO or REPO+FILE or --wipe alltogether."
