{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.Preprocess (preprocess) where

import Control.Monad

import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (State, evalState, get)
import Data.Default
import System.FilePath ((</>))

import Biegunka.Language.External
import Biegunka.Language.Internal


data S = S
  { _root         :: FilePath
  , _source       :: FilePath
  , _profile_name :: String
  , _source_name  :: String
  , _order        :: Int
  } deriving (Show, Read, Eq, Ord)

instance Default S where
  def = S
    { _root         = def
    , _source       = def
    , _profile_name = def
    , _source_name  = def
    , _order        = 1
    }


makeLenses ''S


-- | Infect free monad with state:
--
-- * Path to root
-- * Path to current source
preprocess :: Script Profiles
           -> FilePath
           -> [IL]
preprocess s r = evalState (concatMapM stepP $ toListP s) (def & root .~ r)


stepP :: EL Profiles () -> State S [IL]
stepP (EP n s _) = do
  profile_name .= n
  xs <- concatMapM stepS $ toListS s
  return xs
stepP (EW w _) = return [IW w]

stepS :: EL Sources () -> State S [IL]
stepS (ES t u d s a ()) = do
  S r src pn sn o <- get
  source_name .= u
  source .= r </> d
  order .= 1
  xs <- mapM stepF $ toListF s
  o' <- use order
  return $ IS (r </> d) t (a $ r </> d) o' pn u : xs
stepS (EW w _) = return [IW w]

stepF :: EL Files () -> State S IL
stepF (EF (Link s d) ()) = do
  S r src pn sn o <- get
  order += 1
  return $ IA (Link (src </> s) (r </> d)) o pn sn
stepF (EF (Copy s d) ()) = do
  S r src pn sn o <- get
  order += 1
  return $ IA (Copy (src </> s) (r </> d)) o pn sn
stepF (EF (Template s d t) ()) = do
  S r src pn sn o <- get
  order += 1
  return $ IA (Template (src </> s) (r </> d) t) o pn sn
stepF (EF (Shell d c) ()) = do
  S r s pn sn o <- get
  order += 1
  return $ IA (Shell (s </> d) c) o pn sn
stepF (EW w _) = return $ IW w


toListP :: Script Profiles -> [EL Profiles ()]
toListP (Free (EP n s x)) = EP n s () : toListP x
toListP (Free (EW t x))   = EW t ()   : toListP x
toListP (Pure _)          = []

toListS :: Script Sources -> [EL Sources ()]
toListS (Free (ES t u p s f x)) = ES t u p s f () : toListS x
toListS (Free (EW w x))         = EW w ()         : toListS x
toListS (Pure _)                = []

toListF :: Script Files -> [EL Files ()]
toListF (Free (EF a x)) = EF a () : toListF x
toListF (Free (EW w x)) = EW w () : toListF x
toListF (Pure _)        = []


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
