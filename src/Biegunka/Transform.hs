{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Transform external language into internal one
module Biegunka.Transform (fromEL, simplified) where

import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (State, evalState, get)
import Data.Default

import Biegunka.Language


-- | Transformation state
data Transformation = Transformation
  { _profileName :: String   -- ^ Profile name
  , _sourceName  :: String   -- ^ Source name
  , _order       :: Int      -- ^ Order number
  } deriving (Show, Read, Eq, Ord)

instance Default Transformation where
  def = Transformation
    { _profileName = def
    , _sourceName  = def
    , _order       = 1
    }

makeLenses ''Transformation


-- | Given user defined biegunka script preprocess it into something usable
--
-- Returns internal language "instructions" littered with information used later
fromEL :: Free (EL a Profiles) () -> [IL]
fromEL = return . (`evalState` def) . fmap (IT . chained) . traverse step . toList


-- | Transform Profiles layer
step :: EL a s () -> State Transformation IL
step (EP _ (Profile n) s _) = do
  profileName .= n
  xs <- mapM step $ toList s
  return $ IT (IP n : chained xs)
step (ES _ (Source t u d a) s ()) = do
  Transformation pn _ _ <- get
  sourceName .= u
  order .= 0
  xs <- mapM step $ toList s
  om <- use order
  let ys = map (\(IA a' o _ pn' sn) -> IA a' o om pn' sn) xs
  return $ IT (chained $ IS d t (a d) pn u : ys)
step (EA _ (Link s d) ()) = do
  Transformation pn sn _ <- get
  o <- order <+= 1
  return $ IA (Link s d) o 0 pn sn
step (EA _ (Copy s d) ()) = do
  Transformation pn sn _ <- get
  o <- order <+= 1
  return $ IA (Copy s d) o 0 pn sn
step (EA _ (Template s d t) ()) = do
  Transformation pn sn _ <- get
  o <- order <+= 1
  return $ IA (Template s d t) o 0 pn sn
step (EA _ (Shell d c) ()) = do
  Transformation pn sn _ <- get
  o <- order <+= 1
  return $ IA (Shell d c) o 0 pn sn
step (EW w _) = return $ IW w


toList :: Free (EL a s) x -> [EL () s ()]
toList (Free (EP _ p i x)) = EP () p i () : toList x
toList (Free (ES _ s i x)) = ES () s i () : toList x
toList (Free (EW   t   x)) = EW    t   () : toList x
toList (Free (EA _ a x))   = EA () a   () : toList x
toList (Pure _)          = []


-- | Merge chained instructions
chained :: [IL] -> [IL]
chained (IT xs : IW Chain : ys) = let (IT ws : zs) = chained ys in IT (xs ++ ws) : zs
chained (x : ys) = x : chained ys
chained [] = []

simplified :: [IL] -> [IL]
simplified (IT xs : ys) = simplified xs ++ simplified ys
simplified (x : xs) = x : simplified xs
simplified [] = []
