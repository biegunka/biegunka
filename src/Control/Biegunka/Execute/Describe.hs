{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Describe execution I/O actions
module Control.Biegunka.Execute.Describe
  ( -- * General description formatting
    runChanges
    -- * Specific description formatting
  , describeTerm, removal
  , sourceIdentifier
  , prettyDiff
  ) where

import           Control.Exception (SomeException)
import           Data.Bool (bool)
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty((:|)))
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (mempty, mconcat)
#endif
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import           Text.Printf (printf)

import           Control.Lens
import           System.Process (CmdSpec(..))

import           Control.Biegunka.Namespace (Db, Namespaces)
import qualified Control.Biegunka.Namespace as Ns
import           Control.Biegunka.Language
import           Control.Biegunka.Script
import           Control.Biegunka.Patience (Hunk(..), Judgement(..), judgement)

-- | Describe an action and its outcome.
describeTerm
  :: Retries
  -> Either SomeException (Maybe String)
  -> Bool
  -> Term Annotate s a
  -> String
describeTerm (Retries n) mout withSource ta =
  case mout of
    Left e -> unlines .
      (if withSource then (prefixf ta :) else id) $
        ("  * " ++ doc
                ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
                ++ printf " [%sexception%s]" red reset)
        : map (\l -> printf "    %s%s%s" red l reset) (lines (show e))
    Right Nothing -> unlines $
      (if withSource then (prefixf ta :) else id)
        [ "  * " ++ doc
                 ++ " (up-to-date)"
                 ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
        ]
    Right (Just out) -> unlines $
      (if withSource then (prefixf ta :) else id)
        [ "  * " ++ doc
                 ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
                 ++ printf "\n    %s- %s%s" green out reset
        ]
 where
  prefixf :: Term Annotate s a -> String
  prefixf t = case sourceIdentifier t of
    Nothing -> ""
    Just (url :| ns) ->
      List.intercalate "::" (reverse (printf "[%s]" url : ns))

  doc = case ta of
    TS _ (Source t _ d _) _ _  ->
      printf "%s source[%s] update" t d
    TA _ a _ ->
      case a of
        Link s d ->
          printf "symlink[%s] update (point to [%s])" d s
        Copy s d ->
          printf "file[%s] update (copy [%s])" d s
        Template s d ->
          printf "file[%s] update (from template [%s])" d s
        Command p (ShellCommand c) ->
          printf "execute[%s] (from [%s])" c p
        Command p (RawCommand c as) ->
          printf "execute[%s] (from [%s])" (unwords (c : as)) p
    _ -> ""

-- | Note that the components are in the reverse order.
sourceIdentifier :: Term Annotate s a -> Maybe (NonEmpty String)
sourceIdentifier = \case
  TS (AS { asSegments }) (Source _ url _ _) _ _ -> Just (url :| asSegments)
  TA (AA { aaSegments, aaURI }) _ _ -> Just (aaURI :| aaSegments)
  TWait _ _ -> Nothing

prettyDiff :: [Hunk Text] -> String
prettyDiff =
  toString . unline . (mempty :) . map (prettyHunk . fmap Builder.fromLazyText)

prettyHunk :: Hunk Builder -> Builder
prettyHunk (Hunk n i m j ls) = unline (prettyHeader : map prettyLine ls)
 where
  prettyHeader = Builder.fromString (printf "      %s@@ -%d,%d +%d,%d @@" reset n i m j)

prettyLine :: Judgement Builder -> Builder
prettyLine j = mconcat
  [ Builder.fromString "      "
  , judgement (decorate red '-') (decorate green '+') (decorate reset ' ') j
  ]
 where
  decorate col ch x = mconcat [Builder.fromString col, Builder.singleton ch, x]

toString :: Builder -> String
toString = Text.unpack . Builder.toLazyText

unline :: [Builder] -> Builder
unline = mconcat . List.intersperse (Builder.singleton '\n')

-- | Describe changes which will happen after the run
runChanges :: Db -> Namespaces -> String
runChanges db gs = unlines $ "" : concatMap about
  [ ("deleted files",    red,    map Ns.filePath df)
  , ("deleted sources",  red,    map Ns.sourcePath ds)
  , ("modified files",   yellow, map Ns.filePath mf)
  , ("modified sources", yellow, map Ns.sourcePath ms)
  , ("added files",      green,  map Ns.filePath nf)
  , ("added sources",    green,  map Ns.sourcePath ns)
  ] ++ [""]
 where
  about (msg, color, xs) = case length xs of
    0 -> []
    n -> printf "%s (%d):" msg n : map (\x -> "  " ++ color ++ x ++ reset) xs
  (df, mf, nf) = changes Ns.filePath (Ns.files (view Ns.namespaces db)) (Ns.files gs)
  (ds, ms, ns) = changes Ns.sourcePath (Ns.sources (view Ns.namespaces db)) (Ns.sources gs)

-- | /O(n^2)/
--
-- I have no idea if it works.
changes :: (Eq a, Ord b) => (a -> b) -> [a] -> [a] -> ([a], [a], [a])
changes f xs ys = (xsys, ms, ysxs)
 where
  ms =
    map head (filter (not . null . drop 1 . List.nub)
                     (List.groupBy ((==) `on` f)
                                   (List.sortBy (compare `on` f)
                                                (xs ++ ys))))
  xsys = List.deleteFirstsBy ((==) `on` f) xs ys
  ysxs = List.deleteFirstsBy ((==) `on` f) ys xs

-- | Describe file or directory removal
removal :: FilePath -> String
removal = printf "Removing: %s\n"

red :: String
red = "\ESC[31;2m"

yellow :: String
yellow = "\ESC[33;2m"

green :: String
green = "\ESC[32;2m"

reset :: String
reset = "\ESC[0m"
