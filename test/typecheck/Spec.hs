{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Data.Foldable (for_)
import qualified Data.List as List
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Prelude hiding (tail)
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>), takeFileName)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec


main :: IO ()
main = do
  ghc <- fmap (fromMaybe "ghc") (lookupEnv "NIX_GHC")
  shouldCompile <- ls "./test/typecheck/should_compile"
  shouldFail <- ls "./test/typecheck/should_fail"
  testCases <- traverse testCase (shouldCompile ++ shouldFail)
  hspec $
    for_ testCases $ \case
      Success file ->
        it ("‘" <> file <> "’ compiles") $ do
          (ec, _out, err) <- callCompiler ghc file
          (ec, err) `shouldBe` (ExitSuccess, "")
      Failure file errExcerpt ->
        it ("‘" <> file <> "’ does not compile") $ do
          (ec, _out, err) <- callCompiler ghc file
          ec `shouldBe` ExitFailure 1
          lines err `shouldContain` errExcerpt

ls :: FilePath -> IO [FilePath]
ls dir =
  fmap (map (\file -> dir </> file) . filter (`notElem` [".", ".."]))
       (getDirectoryContents dir)

callCompiler :: FilePath -> FilePath -> IO (ExitCode, String, String)
callCompiler ghc file =
  readProcessWithExitCode
    "cabal"
    ["exec", "--", "env", "LANG=C", ghc, "-O0", "-fno-code", "-isrc", "-idist/build/autogen", file]
    ""

-- | A test case is either a file that compiles successfully
-- or a file that fails to compile and the compile failure's
-- stderr excerpt.
data TestCase
  = Success FilePath
  | Failure FilePath [String]
    deriving (Show, Eq)

testCase :: FilePath -> IO TestCase
testCase file
  | "OK" `List.isPrefixOf` fileName = return (Success file)
  | "Fail" `List.isPrefixOf` fileName =
    do contents <- readFile file
       case parseStderrExcerpt contents of
         Nothing -> error ("stderr excerpt is missing in ‘" <> file <> "’")
         Just excerpt -> return (Failure file excerpt)
  | otherwise = error ("‘" <> file <> "’ has an unknown filename prefix")
 where
  fileName = takeFileName file

parseStderrExcerpt :: String -> Maybe [String]
parseStderrExcerpt =
  tail . mapMaybe (List.stripPrefix "-- ") . dropWhile (/= "-- STDERR") . lines

tail :: [a] -> Maybe [a]
tail (_ : xs) = Just xs
tail []       = Nothing
