{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Transform external language into internal one
module Biegunka.Transform (fromEL, simplified) where

import Control.Applicative

import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (State, evalState, get)
import Data.Default
import System.FilePath ((</>))

import Biegunka.Language


-- | Transformation state
data S = S
  { _root        :: FilePath -- ^ Biegunka root
  , _source      :: FilePath -- ^ Source root
  , _profileName :: String   -- ^ Profile name
  , _sourceName  :: String   -- ^ Source name
  , _order       :: Int      -- ^ Order number
  } deriving (Show, Read, Eq, Ord)

instance Default S where
  def = S
    { _root        = def
    , _source      = def
    , _profileName = def
    , _sourceName  = def
    , _order       = 1
    }

makeLenses ''S


-- | Given user defined biegunka script preprocess it into something usable
--
-- Returns internal language "instructions" littered with information used later
fromEL :: Script Profiles -> FilePath -> [IL]
fromEL s r = return . (`evalState` (def & root .~ r)) $ stepping stepP s
 where
  stepping :: (Folding s, Applicative m) => (EL s () -> m IL) -> Script s -> m IL
  stepping step = fmap (IT . chained) . traverse step . toList


-- | Transform Profiles layer
stepP :: EL Profiles () -> State S IL
stepP (EP n s _) = do
  profileName .= n
  xs <- mapM stepS $ toList s
  return $ IT (IP n : chained xs)
stepP (EW w _) = return $ IW w

-- | Transform Sources layer
stepS :: EL Sources () -> State S IL
stepS (ES t u d s a ()) = do
  S r _ pn _ _ <- get
  sourceName .= u
  source .= r </> d
  order .= 0
  xs <- mapM stepF $ toList s
  om <- use order
  let ys = map (\(IA a' o _ pn' sn) -> IA a' o om pn' sn) xs
  return $ IT (chained $ IS (r </> d) t (a $ r </> d) pn u : ys)
stepS (EW w _) = return $ IW w

-- | Transform Files layer
stepF :: EL Actions () -> State S IL
stepF (EA (Link s d) ()) = do
  S r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Link (src </> s) (r </> d)) o 0 pn sn
stepF (EA (Copy s d) ()) = do
  S r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Copy (src </> s) (r </> d)) o 0 pn sn
stepF (EA (Template s d t) ()) = do
  S r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Template (src </> s) (r </> d) t) o 0 pn sn
stepF (EA (Shell d c) ()) = do
  S _ s pn sn _ <- get
  o <- order <+= 1
  return $ IA (Shell (s </> d) c) o 0 pn sn
stepF (EW w _) = return $ IW w


class Folding s where
  toList :: Script s -> [EL s ()]

instance Folding Profiles where
  toList (Free (EP n s x)) = EP n s () : toList x
  toList (Free (EW t x))   = EW t ()   : toList x
  toList (Pure _)          = []

instance Folding Sources where
  toList (Free (ES t u p s f x)) = ES t u p s f () : toList x
  toList (Free (EW w x))         = EW w ()         : toList x
  toList (Pure _)                = []

instance Folding Actions where
  toList (Free (EA a x)) = EA a () : toList x
  toList (Free (EW w x)) = EW w () : toList x
  toList (Pure _)        = []

-- | Merge chained instructions
chained :: [IL] -> [IL]
chained (IT xs : IW Chain : IT ys : zs) = chained $ IT (xs ++ ys) : zs -- Wrong associativity!
chained (x : zs) = x : chained zs
chained [] = []

simplified :: [IL] -> [IL]
simplified (IT xs : ys) = simplified xs ++ simplified ys
simplified (x : xs) = x : simplified xs
simplified [] = []
