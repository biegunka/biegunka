{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Transform external language into internal one
module Biegunka.Transform (fromEL) where

import Control.Monad

import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (State, evalState, get)
import Data.Default
import System.FilePath ((</>))

import Biegunka.Language.External
import Biegunka.Language.Internal


-- | Transformation state
data S = S
  { _root         :: FilePath -- ^ Biegunka root
  , _source       :: FilePath -- ^ Source root
  , _profile_name :: String   -- ^ Profile name
  , _source_name  :: String   -- ^ Source name
  , _order        :: Int      -- ^ Order number
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


-- | Given user defined biegunka script preprocess it into something usable
--
-- Returns internal language "instructions" littered with information used later
fromEL :: Script Profiles
       -> FilePath
       -> [IL]
fromEL s r = evalState (concatMapM stepP $ toListP s) (def & root .~ r)


-- | Transform Profiles layer
stepP :: EL Profiles () -> State S [IL]
stepP (EP n s _) = do
  profile_name .= n
  xs <- concatMapM stepS $ toListS s
  return xs
stepP (EW w _) = return [IW w]

-- | Transform Sources layer
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

-- | Transform Files layer
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


-- | Folds Profile layer
toListP :: Script Profiles -> [EL Profiles ()]
toListP (Free (EP n s x)) = EP n s () : toListP x
toListP (Free (EW t x))   = EW t ()   : toListP x
toListP (Pure _)          = []

-- | Folds Source layer
toListS :: Script Sources -> [EL Sources ()]
toListS (Free (ES t u p s f x)) = ES t u p s f () : toListS x
toListS (Free (EW w x))         = EW w ()         : toListS x
toListS (Pure _)                = []

-- | Folds Files layer
toListF :: Script Files -> [EL Files ()]
toListF (Free (EF a x)) = EF a () : toListF x
toListF (Free (EW w x)) = EW w () : toListF x
toListF (Pure _)        = []


-- | This isn't defined in Control.Monad
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
