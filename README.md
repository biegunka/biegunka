# biegunka [![Build Status](https://secure.travis-ci.org/biegunka/biegunka-core.png?branch=master)](http://travis-ci.org/biegunka/biegunka-core)
Configurations management library.

## Install
```
% git clone git@github.com:biegunka/biegunka-core
% git clone git@github.com:biegunka/biegunka-git
% cabal[-dev] install biegunka-core/biegunka-core.cabal biegunka-git/biegunka-git.cabal
```

## Example
The simplest yet meaningful biegunka script

```haskell
import Data.Monoid
import Biegunka
import Biegunka.Source.Git

main :: IO ()
main = biegunka def (pretend `mappend` execute) $
  profile "my-configs" $
    git "https://my.server.with.configs.com/dotfiles" "/home/user/.dotfiles" $
	  link "xmonad.hs" "/home/user/.xmonad/xmonad.hs"
```
We use it as an example to get high level intuition about what `biegunka` is.

--

```haskell
import Data.Monoid
import Biegunka
import Biegunka.Source.Git
```

Necessary imports:
  * `Data.Monoid` contains `mappend` which is useful for composition
  * `Biegunka` module contains core functionality such as copying or linking files from sources
  * `Biegunka.Source.Git` enables git support

**Note**: there are other `Source` modules like `Biegunka.Source.Darcs` or `Biegunka.Source.Tar` for different kinds of sources you may want to use.

--

```haskell
main = biegunka def (pretend `mappend` execute) $
```
Biegunka scripts are executed by interpreters. Here we see 2 of them:
 * `pretend` assumes everything went without errors and prints script stats based on that assumption
 * `execute` does real work of getting sources and moving files

**Note**: interpreters compose with `Data.Monoid.mappend`.

--

```haskell
  profile "my-configs" $
```

Profiles are groups of related sources. Each profile gets its own database, so changes in one profile do not affect others.

--

```haskell
    git "https://my.server.with.configs.com/dotfiles" "/home/user/.dotfiles" $
```

Sources are "places with files". `git` takes git repository at `https://my.server.with.configs.com/dotfiles.git` and clones it to `/home/user/.dotfiles`

--

```haskell
      link "xmonad.hs" "/hone/user/.xmonad/xmonad.hs"
```
File operations layer provides functions to work with sources files, for example, `link` links `/home/user/.xmonad/xmonad.hs` to `/home/user/.dotfiles/xmonad.hs`.

## More examples

  * [Wiki][1] has a handful of examples
  * biegunka [authors][2] [use][3] it themselves

 [1]: https://github.com/biegunka/biegunka-core/wiki/Examples
 [2]: https://github.com/supki/.dotfiles/blob/master/manage/Main.hs
 [3]: https://github.com/dmalikov/dotfiles/blob/master/install-biegunka.hs
