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
fromEL :: Script Profiles
       -> FilePath
       -> [IL]
fromEL s r = evalState (concatMapM stepP $ toListP s) (def & root .~ r)


-- | Transform Profiles layer
stepP :: EL Profiles () -> State S [IL]
stepP (EP n s _) = do
  profileName .= n
  concatMapM stepS $ toListS s
stepP (EW w _) = return [IW w]

-- | Transform Sources layer
stepS :: EL Sources () -> State S [IL]
stepS (ES t u d s a ()) = do
  S r _ pn _ _ <- get
  sourceName .= u
  source .= r </> d
  order .= 1
  xs <- mapM stepF $ toListF s
  o' <- use order
  return $ IS (r </> d) t (a $ r </> d) o' pn u : xs
stepS (EW w _) = return [IW w]

-- | Transform Files layer
stepF :: EL Actions () -> State S IL
stepF (EA (Link s d) ()) = do
  S r src pn sn o <- get
  order += 1
  return $ IA (Link (src </> s) (r </> d)) o pn sn
stepF (EA (Copy s d) ()) = do
  S r src pn sn o <- get
  order += 1
  return $ IA (Copy (src </> s) (r </> d)) o pn sn
stepF (EA (Template s d t) ()) = do
  S r src pn sn o <- get
  order += 1
  return $ IA (Template (src </> s) (r </> d) t) o pn sn
stepF (EA (Shell d c) ()) = do
  S _ s pn sn o <- get
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
toListF :: Script Actions -> [EL Actions ()]
toListF (Free (EA a x)) = EA a () : toListF x
toListF (Free (EW w x)) = EW w () : toListF x
toListF (Pure _)        = []


-- | This isn't defined in Control.Monad
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
