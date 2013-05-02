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
data Transformation = Transformation
  { _root        :: FilePath -- ^ Biegunka root
  , _source      :: FilePath -- ^ Source root
  , _profileName :: String   -- ^ Profile name
  , _sourceName  :: String   -- ^ Source name
  , _order       :: Int      -- ^ Order number
  } deriving (Show, Read, Eq, Ord)

instance Default Transformation where
  def = Transformation
    { _root        = def
    , _source      = def
    , _profileName = def
    , _sourceName  = def
    , _order       = 1
    }

makeLenses ''Transformation


-- | Given user defined biegunka script preprocess it into something usable
--
-- Returns internal language "instructions" littered with information used later
fromEL :: Script Profiles () -> FilePath -> [IL]
fromEL s r = return . (`evalState` (def & root .~ r)) $ stepping stepP s
 where
  stepping :: (Folding s, Applicative m) => (EL s () -> m IL) -> Script s () -> m IL
  stepping step = fmap (IT . chained) . traverse step . toList


-- | Transform Profiles layer
stepP :: EL Profiles () -> State Transformation IL
stepP (EP (Profile n) s _) = do
  profileName .= n
  xs <- mapM stepS $ toList s
  return $ IT (IP n : chained xs)
stepP (EW w _) = return $ IW w

-- | Transform Sources layer
stepS :: EL Sources () -> State Transformation IL
stepS (ES (Source t u d a) s ()) = do
  Transformation r _ pn _ _ <- get
  sourceName .= u
  source .= r </> d
  order .= 0
  xs <- mapM stepF $ toList s
  om <- use order
  let ys = map (\(IA a' o _ pn' sn) -> IA a' o om pn' sn) xs
  return $ IT (chained $ IS (r </> d) t (a $ r </> d) pn u : ys)
stepS (EW w _) = return $ IW w

-- | Transform Files layer
stepF :: EL Actions () -> State Transformation IL
stepF (EA (Link s d) ()) = do
  Transformation r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Link (src </> s) (r </> d)) o 0 pn sn
stepF (EA (Copy s d) ()) = do
  Transformation r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Copy (src </> s) (r </> d)) o 0 pn sn
stepF (EA (Template s d t) ()) = do
  Transformation r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Template (src </> s) (r </> d) t) o 0 pn sn
stepF (EA (Shell d c) ()) = do
  Transformation _ s pn sn _ <- get
  o <- order <+= 1
  return $ IA (Shell (s </> d) c) o 0 pn sn
stepF (EW w _) = return $ IW w


class Folding s where
  toList :: Script s a -> [EL s ()]

instance Folding Profiles where
  toList (Script m) = go m
   where
    go :: Free (EL Profiles) a -> [EL Profiles ()] -- to avoid non-exhaustive pattern match warning
    go (Free (EP p i x)) = EP p i () : go x
    go (Free (EW t   x)) = EW t   () : go x
    go (Pure _)          = []

instance Folding Sources where
  toList (Script m) = go m
   where
    go :: Free (EL Sources) a -> [EL Sources ()]
    go (Free (ES s i x)) = ES s i () : go x
    go (Free (EW w   x)) = EW w   () : go x
    go (Pure _)          = []

instance Folding Actions where
  toList (Script m) = go m
   where
    go :: Free (EL Actions) a -> [EL Actions ()]
    go (Free (EA a x)) = EA a () : go x
    go (Free (EW w x)) = EW w () : go x
    go (Pure _)        = []

-- | Merge chained instructions
chained :: [IL] -> [IL]
chained (IT xs : IW Chain : IT ys : zs) = chained $ IT (xs ++ ys) : zs -- Wrong associativity!
chained (x : zs) = x : chained zs
chained [] = []

simplified :: [IL] -> [IL]
simplified (IT xs : ys) = simplified xs ++ simplified ys
simplified (x : xs) = x : simplified xs
simplified [] = []
