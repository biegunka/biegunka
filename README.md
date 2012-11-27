# Biegunka
[![Build Status](https://secure.travis-ci.org/biegunka/biegunka-core.png?branch=master)](http://travis-ci.org/biegunka/biegunka-core)  
This library provides tools to write configuration files management scripts.
*Requires GHC 7.6*

## Installing
```
cabal install
```

## Getting started
The simplest yet meaningful Biegunka script

```haskell
import Biegunka
import Biegunka.Source.Git

main :: IO ()
main = execute $
  profile "my-configs" $
    git "https://my.server.with.configs.com/dotfiles" "/home/user/.dotfiles" $
	  link "xmonad.hs" ".xmonad/xmonad.hs"
```
This example doesn't do very much but is useful to get high level intuition about what Biegunka scripts are. Let's start line by line.

```haskell
import Biegunka
import Biegunka.Source.Git
```
Needed imports. `Biegunka` imports infrastructure for scripts, `Biegunka.Source.Git` allows to use `git` function to clone and update git repository.

```haskell
main = execute $
```
Any Biegunka script should be executed by some interpreter, `execute` is one of them: it does *real* work.

```haskell
  profile "my-configs" $
```
Profiles are useful for grouping repositories together. They don't have any meaning besides that (yet) and exist for clarity of the code.

```haskell
    git "https://my.server.with.configs.com/dotfiles" "/home/user/.dotfiles" $
```
Okay, that's where things become interesting. This command creates `/home/user/.dotfiles` directory and clones `https://my.server.with.configs.com/dotfiles.git` there. By default it uses `origin` remote and `master` branch.

```haskell
	  link "xmonad.hs" ".xmonad/xmonad.hs"
```
This command links `/home/user/.xmonad/xmonad.hs` to `/home/user/.dotfiles/xmonad.hs` creating necessary directories.

After execution of the script user has cloned git repository at `/home/user/.dotfiles` and XMonad config at `/home/user/.xmonad/xmonad.hs` linked from it.

## More sophisticated example
See [wiki][1].

 [1]: https://github.com/supki/biegunka/wiki/Examples
