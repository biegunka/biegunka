{-# LANGUAGE DataKinds #-}
-- -*- mode: haskell -*-
-- vi: set ft=haskell :

-- Import biegunka library primitives
import Control.Biegunka
-- Import git support (most likely you will need it)
import Control.Biegunka.Source.Git
-- Auxiliary functions, no need to know what 'lens' is
import Control.Lens


main :: IO ()
main = do
  -- Generate and use options parser to get environment to run scripts in
  runBiegunka <- runner_
  -- Make location of all sources relative to `~`
  runBiegunka (set runRoot "~") script

-- Biegunka script stub
script :: Script 'Sources ()
script = do
  namespace "my-dotfiles" $ do
    -- Your dotfiles repositories go here
    git (origin "git@github.com:user/dotfiles" . path "somewhere/under/~") $
      -- What exactly to do with the cloned repository?
      pass
    pass
  -- Other namespaces go there
  pass
