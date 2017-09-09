{-# Language CPP #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import           Distribution.Simple (defaultMainWithHooks, simpleUserHooks, buildHook)
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import           Distribution.Simple.Utils (rewriteFile)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), (<.>))
import           System.IO.Error (catchIOError)
import           System.Process (readProcess)

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi uh bf -> do generateGit_biegunka lbi; buildHook simpleUserHooks pd lbi uh bf
  }

generateGit_biegunka :: LocalBuildInfo -> IO ()
generateGit_biegunka lbi =
  do putStrLn ("Generating " ++ git_biegunka ++ " ...")
     createDirectoryIfMissing True autogen
     hash <- gitHash
     rewriteFile git_biegunka (unlines
       [ "module Git_biegunka (hash) where"
       , ""
       , "hash :: String"
       , "hash = " ++ show hash
       ])
 where
#if MIN_VERSION_Cabal(1,25,0)
  autogen = autogenPackageModulesDir lbi
#else
  autogen = autogenModulesDir lbi
#endif
  git_biegunka = autogen </> "Git_biegunka" <.> "hs"
{-# ANN generateGit_biegunka "HLint: ignore Use camelCase" #-}

gitHash :: IO String
gitHash =
  catchIOError (fmap sanitize (readProcess "git" ["describe", "--always", "--dirty=-dirty"] ""))
               (\_ -> return "unknown")
 where
  sanitize = List.dropWhileEnd Char.isSpace
