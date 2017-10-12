{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Describe execution I/O actions
module Control.Biegunka.Execute.Describe
  ( prettyTerm, removal
  , sourceIdentifier
  , prettyDiff
  ) where

import           Control.Exception (SomeException)
import           Data.Bool (bool)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import           Text.Printf (printf)

import           Control.Biegunka.Language
import           Control.Biegunka.Script
import           Control.Biegunka.Patience (Hunk(..), Judgement(..), judgement)


prettyTerm
  :: Retries
  -> Either SomeException [DiffItem]
  -> Bool
  -> TermF Annotate s a
  -> String
prettyTerm (Retries n) mout withSource ta =
   unlines . (if withSource then (prefixf ta :) else id) $ case mout of
    Left e ->
      ("  * " ++ doc
              ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
              ++ printf " [%sexception%s]" red reset)
      : map (\l -> printf "    %s%s%s" red l reset) (lines (show e))
    Right [] ->
      [ "  * " ++ doc
               ++ " (up-to-date)"
               ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
      ]
    Right diffItems ->
      ("  * " ++ doc
              ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0))
      : concatMap prettyDiffItem diffItems
 where
  prefixf :: TermF Annotate s a -> String
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
        Decrypt s m d ->
          printf "file[%s] update (decrypting from [%s] with meta from [%s])" d s m
        Template s d ->
          printf "file[%s] update (from template [%s])" d s
        Command p c as ->
          printf "execute[%s] (from [%s])" (unwords (c : as)) p
    _ -> ""

prettyDiffItem :: DiffItem -> [String]
prettyDiffItem DiffItem { diffItemHeader, diffItemBody } =
  printf "    %s- %s%s" green diffItemHeader reset : map (mappend "      ") (lines diffItemBody)

-- | Note that the components are in the reverse order.
sourceIdentifier :: TermF Annotate s a -> Maybe (NonEmpty String)
sourceIdentifier = \case
  TS AS { asSegments } (Source _ url _ _) _ _ -> Just (url :| asSegments)
  TA AA { aaSegments, aaUrl } _ _ -> Just (aaUrl :| aaSegments)
  TWait _ _ -> Nothing

prettyDiff :: [Hunk Text] -> String
prettyDiff =
  nonempty " (no diff)" (toString . unline . (mempty :) . map (prettyHunk . fmap Builder.fromLazyText))

nonempty :: b -> ([a] -> b) -> [a] -> b
nonempty z _ [] = z
nonempty _ f xs = f xs

prettyHunk :: Hunk Builder -> Builder
prettyHunk (Hunk n i m j ls) = unline (prettyHeader : map prettyLine ls)
 where
  prettyHeader = Builder.fromString (printf "%s@@ -%d,%d +%d,%d @@" reset n i m j)

prettyLine :: Judgement Builder -> Builder
prettyLine =
  judgement (decorate red '-') (decorate green '+') (decorate reset ' ')
 where
  decorate col ch x = mconcat [Builder.fromString col, Builder.singleton ch, x]

toString :: Builder -> String
toString = Text.unpack . Builder.toLazyText

unline :: [Builder] -> Builder
unline = mconcat . List.intersperse (Builder.singleton '\n')

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
