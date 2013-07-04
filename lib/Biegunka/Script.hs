{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Configuration script machinery
module Biegunka.Script
  ( Script(..), SS, SA(..), liftS, annotate, rewind, URI, sourced, actioned, constructDestinationFilepath
  , token, app, source, sourceURL, order
  , runScript, evalScript
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (when)
import Data.List (isSuffixOf)

import Control.Lens
import Control.Monad.Free (Free(..), iter, liftF)
import Control.Monad.State (MonadState(..), StateT(..), State, execState, lift, state)
import Data.Default (Default(..))
import System.FilePath.Lens

import Biegunka.Language


-- | Language term annotation depending on their scope
data family SA (sc :: Scope) :: *
data instance SA Profiles = SAP { sapToken :: Int }
data instance SA Sources  = SAS { sasToken :: Int }
data instance SA Actions  = SAA { saaURI :: URI, saaOrder :: Int, saaMaxOrder :: Int }


-- | Newtype used to provide better error messages for type errors in DSL
newtype Script s a = Script { unScript :: StateT SS (Free (Term SA s)) a }

instance Functor (Script s) where
  fmap f (Script m) = Script (fmap f m)
  {-# INLINE fmap #-}

instance Applicative (Script s) where
  pure v = Script (pure v)
  {-# INLINE pure #-}
  Script m <*> Script n = Script (m <*> n)
  {-# INLINE (<*>) #-}

instance Monad (Script s) where
  return v = Script (return v)
  {-# INLINE return #-}
  Script m >>= f = Script (m >>= unScript . f)
  {-# INLINE (>>=) #-}

instance Default a => Default (Script s a) where
  def = return def
  {-# INLINE def #-}

-- | Get DSL and resulting state from 'Script'
runScript :: SS -> Script s a -> Free (Term SA s) (a, SS)
runScript s = (`runStateT` s) . unScript
{-# INLINE runScript #-}

-- | Get DSL from 'Script'
evalScript :: SS -> Script s a -> Free (Term SA s) a
evalScript = (fmap fst .) . runScript
{-# INLINE evalScript #-}

-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String

-- | Script construction state
data SS = SS
  { _token :: Int       -- ^ Unique term token
  , _app :: FilePath    -- ^ Biegunka root filepath
  , _source :: FilePath -- ^ Source root filepath
  , _sourceURL :: URI   -- ^ Current source url
  , _order :: Int       -- ^ Current action order
  , _maxOrder :: Int    -- ^ Maximum action order in current source
  } deriving (Show, Read)

instance Default SS where
  def = SS
    { _token = 0
    , _app = ""
    , _source = ""
    , _sourceURL = ""
    , _order = 0
    , _maxOrder = 0
    }

makeLensesWith ?? ''SS $ defaultRules & generateSignatures .~ False

-- | Unique token for each 'EP'/'ES'
token :: Lens' SS Int

-- | Biegunka filepath root
app :: Lens' SS FilePath

-- | Current source filepath
source :: Lens' SS FilePath

-- | Current source url
sourceURL :: Lens' SS String

-- | Current action order
order :: Lens' SS Int

-- | Maximum action order in current source
maxOrder :: Lens' SS Int

-- | Lift DSL term to the 'Script'
liftS :: Term SA s a -> Script s a
liftS = Script . lift . liftF
{-# INLINE liftS #-}

-- | Annotate DSL
annotate :: Script s a -> StateT SS (Free (Term SA t)) (Free (Term SA s) a)
annotate i = state $ \s ->
  let r = runScript s i
      ast = fmap fst r
      s' = iter peek $ fmap snd r
  in (ast, s')

-- | Rewind state part pointed by a 'Lens\'' after monadic action execution
rewind :: MonadState s m => Lens' s a -> m b -> m a
rewind l mb = do
  a <- use l
  mb
  a' <- use l
  l .= a
  return a'

-- | Abstract away all plumbing needed to make source
sourced :: String -> URI -> FilePath
        -> Script Actions () -> (FilePath -> IO ()) -> Script Sources ()
sourced ty url path script update = Script $ do
  rfp <- use app
  tok <- use token
  let df = constructDestinationFilepath rfp url path
  source .= df
  sourceURL .= url
  order .= 0
  maxOrder .= size script
  ast <- annotate script
  lift . liftF $ ES (SAS { sasToken = tok }) (S ty url df update) ast ()
  token += 1

-- | 'Actions' scope script size (in actual actions)
size :: Script Actions a -> Int
size = (`execState` 0) . go . evalScript def
 where
  go :: Free (Term SA Actions) a -> State Int ()
  go (Free c@(EA {})) = id %= succ >> go (peek c)
  go (Free c@(EM {})) = go (peek c)
  go (Pure _) = return ()

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> A) -> Script Actions ()
actioned f = Script $ do
  rfp <- use app
  sfp <- use source
  url <- use sourceURL
  o <- order <+= 1
  mo <- use maxOrder
  lift . liftF $ EA (SAA { saaURI = url, saaOrder = o, saaMaxOrder = mo }) (f rfp sfp) ()

-- | Construct destination 'FilePath'
--
-- Trying to be smart: if provided filepath ends in @\/@,
-- then append source filepath's basename
constructDestinationFilepath :: FilePath -> FilePath -> FilePath -> FilePath
constructDestinationFilepath r s d = execState ?? r $ do
  id </>= d
  when ("/" `isSuffixOf` d) $
    id </>= (s^.filename)
