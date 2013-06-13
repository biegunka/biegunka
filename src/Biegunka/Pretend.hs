{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.Pretend (pretend) where

import Control.Monad (when)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Prelude hiding (log)

import Control.Lens
import System.IO
import Text.PrettyPrint.ANSI.Leijen

import Biegunka.DB
import Biegunka.Language (IL(..), A(..), W(..))
import Biegunka.Control (Interpreter(..), logger)
import Biegunka.Transform (fromEL, simplified)


-- | Pretend interpreter
--
-- Doesn't do any IO, so you can't check if script will fail to do IO
--
-- But Pretend can show which changes would be maid if IO will run without errors
--
-- Prints execution log if asked
--
-- @
-- main :: IO ()
-- main = pretend $ do
--   profile ...
--   profile ...
-- @
pretend :: Interpreter
pretend = I $ \c@(view logger -> l) s -> do
  let s' = simplified (fromEL s)
  a <- load c s
  let b = construct s'
  l $ stats a b
  whenM (query l "Print full log?") $
    l (log s' a b)
 where
  whenM ma mb = do
    p <- ma
    when p mb

  query l s = do
    l (s </> "[yN]" <//> colon </> empty)
    c <- getChar'
    l line
    return (c == 'y')

  getChar' = do
    hSetBuffering stdin NoBuffering
    c <- getChar
    hSetBuffering stdin LineBuffering
    return c


stats :: Biegunka -> Biegunka -> Doc
stats a b = vcat $ mapMaybe about
  [ ("added files",     map (yellow  . text) $ filepaths b \\ filepaths a)
  , ("added sources",   map (magenta . text) $ sources b   \\ sources a)
  , ("deleted files",   map (yellow  . text) $ filepaths a \\ filepaths b)
  , ("deleted sources", map (magenta . text) $ sources a   \\ sources b)
  ] ++ [empty]
 where
  about (msg, xs) = case length xs of
    0 -> Nothing
    n -> Just $ nest 2 ((msg </> parens (pretty n) <//> colon) <$> vcat (xs ++ [empty]))


log :: [IL] -> Biegunka -> Biegunka -> Doc
log cs a b = vcat (mapMaybe install cs ++ [empty] ++ uninstall ++ [empty])
 where
  install :: IL -> Maybe Doc
  install (IS p t _ _ u) =
    Just $ green "update" </> text t </> "source" </> cyan (text u) </> "at" </> magenta (text p)
  install (IA (Link src dst) _ _ _ _) = Just . indent 2 $
    yellow (text dst) </> green "links" </> "to" </> magenta (text src)
  install (IA (Copy src dst) _ _ _ _) = Just . indent 2 $
    yellow (text dst) </> "is a" </> green "copy" </> "of" </> magenta (text src)
  install (IA (Template src dst _) _ _ _ _) = Just . indent 2 $
    yellow (text dst) </> "is copied with substituted" </> green "templates" </> "from" </> magenta (text src)
  install (IA (Shell p c) _ _ _ _) = Just . indent 2 $
    green "shell" </> "`" <//> red (text c) <//> "` executed from" </> yellow (text p)
  install (IW w)           = go w
   where
    go (User (Just user)) = Just $ green "change user" </> "to" </> text user
    go (User Nothing)     = Just $ green "change user" </> "back"
    go _                  = Nothing
  install (IT _) = Nothing
  install (IP _) = Nothing

  uninstall :: [Doc]
  uninstall = map ("Delete" </>) $
    map (yellow . text) (filepaths a \\ filepaths b) ++ map (magenta . text) (sources a \\ sources b)
