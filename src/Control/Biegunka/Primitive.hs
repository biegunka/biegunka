{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Language primitives
--
-- Containts 'Actions' layer primitive and modifiers.
-- 'Sources' layer primitives are found in 'Biegunka.Source.*' modules
--
-- All concrete primitives docs assume you have default settings
module Control.Biegunka.Primitive
  ( -- * Actions layer primitives
    link
  , copy
  , template
  , origin
  , path
  , mode
  , owner
  , group
  , register
  , raw
    -- * Modifiers
  , namespace
  , sudo
  , retries
  , reacting
  , prerequisiteOf
  , (<~>)
  ) where

import           Control.Lens
import           Control.Monad.Reader (local)
import qualified Data.Set as Set
import           System.Command.QQ (Eval(..))
import qualified System.Directory.Layout as Layout
import           System.FilePath ((</>))
import qualified System.Posix as Posix

import qualified Control.Biegunka.Language as Language
import           Control.Biegunka.Language hiding (origin, path, mode, owner, group)
import           Control.Biegunka.Script


infixr 7 `prerequisiteOf`, <~>


-- | Namespaces group 'Sources' together; they can be nested.
--
-- Example usage:
--
-- > namespace "dotfiles" $ do
-- >   namespace "mine" $
-- >     git "https://github.com/supki/.dotfiles" ...
-- >       ...
-- >   namespace "not-mine" $
-- >     git "https://github.com/dmalikov/dotfiles" ...
-- >       ...
-- > namespace "experimental" $ do
-- >   git "https://github.com/ekmett/lens"
-- >     ...
namespace :: String -> Script 'Sources a -> Script 'Sources a
namespace segment (Script inner) =
  Script (local (over segments (segment :)) inner)

copy :: (File 'Copy NoOrigin NoPath -> File 'Copy FilePath FilePath) -> Script 'Actions ()
copy f =
  filed (\rfp sfp -> FC (sfp </> origin_) (constructTargetFilePath rfp origin_ path_) mode_ owner_ group_)
 where
  FC origin_ path_ mode_ owner_ group_ = f (FC NoOrigin NoPath Nothing Nothing Nothing)

link :: (File 'Link NoOrigin NoPath -> File 'Link FilePath FilePath) -> Script 'Actions ()
link f =
  filed (\rfp sfp -> FL (sfp </> origin_) (constructTargetFilePath rfp origin_ path_) owner_ group_)
 where
  FL origin_ path_ owner_ group_ = f (FL NoOrigin NoPath Nothing Nothing)

template :: (File 'Template NoOrigin NoPath -> File 'Template FilePath FilePath) -> Script 'Actions ()
template f =
  filed (\rfp sfp -> FT (sfp </> origin_) (constructTargetFilePath rfp origin_ path_) mode_ owner_ group_)
 where
  FT origin_ path_ mode_ owner_ group_ = f (FT NoOrigin NoPath Nothing Nothing Nothing)

origin :: HasOrigin s t a b => b -> s -> t
origin = set Language.origin

path :: HasPath s t a b => b -> s -> t
path = set Language.path

mode :: (s ~ t, t ∈ ['Copy, 'Template]) => Posix.FileMode -> File s a b -> File t a b
mode = set Language.mode . Just

owner :: (s ~ t) => Layout.User -> File s a b -> File t a b
owner = set Language.owner . Just

group :: (s ~ t) => Layout.Group -> File s a b -> File t a b
group = set Language.group . Just

register :: (forall a. File 'Link a NoPath -> File 'Link a FilePath) -> Script 'Actions ()
register f =
  filed (\rfp sfp -> FL sfp (rfp </> path_) owner_ group_)
 where
  FL NoOrigin path_ owner_ group_ = f (FL NoOrigin NoPath Nothing Nothing)

-- -- | Links source to specified filepath
-- --
-- -- > git "https://example.com/source.git" "git/source" $
-- -- >   register "somewhere"
-- --
-- -- Links @~\/git\/source@ to @~\/somewhere@.
-- register :: FilePath -> Script 'Actions ()
-- register dst = actioned (\rfp sfp -> Link sfp (rfp </> dst))

-- | Monomorphised interface to 'sh' quasiquoter for
-- those who do not like @-XTemplateHaskell@ (or @-XQuasiQuotes@)
--
-- > git "https://example.com/source.git" "git/source" $
-- >   raw "/bin/echo" ["-n", "hello"]
--
-- Prints \"hello\" to stdout
raw :: FilePath -> [String] -> Script 'Actions ()
raw = eval

-- | Run the inner commands as root..
sudo :: Script s a -> Script s a
sudo (Script inner) = Script $
  local (set sudoActive True) inner

-- | Change maximum retries count
retries :: Int -> Script s a -> Script s a
retries count (Script inner) = Script $
  set maxRetries (Retries count) `local` inner

-- | Change reaction pattern when retries are all failed
reacting :: React -> Script s a -> Script s a
reacting reaction (Script inner) = Script $
  (set actionReaction reaction . set sourceReaction reaction) `local` inner

-- | Execute scripts sequentially
-- Connects two scripts which forces them to run sequentially one after another.
prerequisiteOf :: Script 'Sources a -> Script 'Sources b -> Script 'Sources b
prerequisiteOf a b = do
  s <- Script peekToken
  a
  t <- Script peekToken
  script (TW (Set.fromList [s .. pred t]) ())
  b

-- | Infix alias for 'prerequisiteOf'
(<~>) :: Script 'Sources a -> Script 'Sources b -> Script 'Sources b
(<~>) = prerequisiteOf
