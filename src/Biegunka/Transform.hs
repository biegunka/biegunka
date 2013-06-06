{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Transform external language into internal one
module Biegunka.Transform (fromEL, simplified) where

import Control.Monad (when)
import Data.List (isSuffixOf)

import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (State, evalState, execState, get)
import Data.Default
import System.FilePath ((</>))
import System.FilePath.Lens

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
fromEL s r = return . (`evalState` (def & root .~ r)) . fmap (IT . chained) . traverse step . toList $ s


-- | Transform Profiles layer
step :: EL s () -> State Transformation IL
step (EP (Profile n) s _) = do
  profileName .= n
  xs <- mapM step $ toList s
  return $ IT (IP n : chained xs)
step (ES (Source t u d a) s ()) = do
  Transformation r _ pn _ _ <- get
  sourceName .= u
  let df = constructDestinationFilepath r u d
  source .= df
  order .= 0
  xs <- mapM step $ toList s
  om <- use order
  let ys = map (\(IA a' o _ pn' sn) -> IA a' o om pn' sn) xs
  return $ IT (chained $ IS df t (a $ df) pn u : ys)
step (EA (Link s d) ()) = do
  Transformation r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Link (src </> s) (constructDestinationFilepath r s d)) o 0 pn sn
step (EA (Copy s d) ()) = do
  Transformation r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Copy (src </> s) (constructDestinationFilepath r s d)) o 0 pn sn
step (EA (Template s d t) ()) = do
  Transformation r src pn sn _ <- get
  o <- order <+= 1
  return $ IA (Template (src </> s) (constructDestinationFilepath r s d) t) o 0 pn sn
step (EA (Shell d c) ()) = do
  Transformation _ s pn sn _ <- get
  o <- order <+= 1
  return $ IA (Shell (s </> d) c) o 0 pn sn
step (EW w _) = return $ IW w


constructDestinationFilepath :: FilePath -> FilePath -> FilePath -> FilePath
constructDestinationFilepath r s d = execState ?? r $ do
  id </>= d
  when ("/" `isSuffixOf` d) $
    id </>= (s^.filename)


toList :: Script s a -> [EL s ()]
toList = go . runScript
 where
  go :: Free (EL s) a -> [EL s ()]
  go (Free (EP p i x)) = EP p i () : go x
  go (Free (ES s i x)) = ES s i () : go x
  go (Free (EW t   x)) = EW t   () : go x
  go (Free (EA a x))   = EA a   () : go x
  go (Pure _)          = []


-- | Merge chained instructions
chained :: [IL] -> [IL]
chained (IT xs : IW Chain : ys) = let (IT ws : zs) = chained ys in IT (xs ++ ws) : zs
chained (x : ys) = x : chained ys
chained [] = []

simplified :: [IL] -> [IL]
simplified (IT xs : ys) = simplified xs ++ simplified ys
simplified (x : xs) = x : simplified xs
simplified [] = []
