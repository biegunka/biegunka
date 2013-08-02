{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Configuration script machinery
module Control.Biegunka.Script
  ( Script(..), AnnotationsState, AnnotationsEnv, Annotate(..)
  , script, annotate, URI, sourced, actioned, constructDestinationFilepath
  , token, app, profiles, profileName, sourcePath, sourceURL, order
  , runScript, runScript', evalScript
  ) where

import Control.Applicative (Applicative(..), (<$))
import Control.Monad (when)
import Data.List (isSuffixOf)

import           Control.Lens hiding (Action)
import           Control.Monad.Free (Free(..), iter, liftF)
import           Control.Monad.State (StateT(..), State, execState)
import           Control.Monad.Reader (ReaderT(..), local)
import           Control.Monad.Trans (lift)
import           Data.Default (Default(..))
import           Data.Copointed (copoint)
import           Data.Set (Set)
import qualified Data.Set as S
import           System.FilePath.Lens

import Control.Biegunka.Language


-- | Language 'Term' annotation depending on their 'Scope'
data family Annotate (sc :: Scope) :: *
data instance Annotate Sources  = AS { asToken :: Int, asProfile :: String }
data instance Annotate Actions  = AA { aaURI :: URI, aaOrder :: Int, aaMaxOrder :: Int }


-- | Newtype used to provide better error messages for type errors in DSL
newtype Script s a = Script
  { unScript :: ReaderT AnnotationsEnv
      (StateT AnnotationsState (Free (Term Annotate s))) a
  }

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
runScript
  :: AnnotationsState
  -> AnnotationsEnv
  -> Script s a
  -> Free (Term Annotate s) (a, AnnotationsState)
runScript as ae (Script s) = runStateT (runReaderT s ae) as
{-# INLINE runScript #-}

-- | Get DSL and resulting state from 'Script'
runScript'
  :: AnnotationsState
  -> AnnotationsEnv
  -> Script s a
  -> (Free (Term Annotate s) a, AnnotationsState)
runScript' as ae s =
  let ast      = runScript as ae s
      (a, as') = iter copoint ast
  in (a <$ ast, as')
{-# INLINE runScript' #-}

-- | Get DSL from 'Script'
evalScript
  :: AnnotationsState
  -> AnnotationsEnv
  -> Script s a
  -> Free (Term Annotate s) a
evalScript = ((fmap fst .) .) . runScript
{-# INLINE evalScript #-}

-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String

-- | Script construction state
data AnnotationsState = AState
  { _token :: Int           -- ^ Unique term token
  , _profiles :: Set String -- ^ Profile name
  , _order :: Int           -- ^ Current action order
  , _maxOrder :: Int        -- ^ Maximum action order in current source
  } deriving (Show, Read)

instance Default AnnotationsState where
  def = AState
    { _token = 0
    , _profiles = S.empty
    , _order = 0
    , _maxOrder = 0
    }

data AnnotationsEnv = AEnv
  { _profileName :: String  -- ^ Profile name
  , _sourcePath :: FilePath -- ^ Source root filepath
  , _sourceURL :: URI       -- ^ Current source url
  , _app :: FilePath        -- ^ Biegunka root filepath
  }

instance Default AnnotationsEnv where
  def = AEnv
    { _profileName = ""
    , _sourcePath = ""
    , _sourceURL = ""
    , _app = ""
    }


makeLensesWith ?? ''AnnotationsState $ defaultRules & generateSignatures .~ False

-- | Unique token for each 'TP'/'TS'
token :: Lens' AnnotationsState Int

-- | All profiles encountered so far
profiles :: Lens' AnnotationsState (Set String)

-- | Current action order
order :: Lens' AnnotationsState Int

-- | Maximum action order in current source
maxOrder :: Lens' AnnotationsState Int

makeLensesWith ?? ''AnnotationsEnv   $ defaultRules & generateSignatures .~ False

-- | Biegunka filepath root
app :: Lens' AnnotationsEnv FilePath

-- | Current profile name
profileName :: Lens' AnnotationsEnv String

-- | Current source filepath
sourcePath :: Lens' AnnotationsEnv FilePath

-- | Current source url
sourceURL :: Lens' AnnotationsEnv String

-- | Lift DSL term to the 'Script'
script :: Term Annotate s a -> Script s a
script = Script . lift . liftF
{-# INLINE script #-}

-- | Annotate DSL
annotate
  :: Script s a
  -> ReaderT AnnotationsEnv (StateT AnnotationsState (Free (Term Annotate t))) (Free (Term Annotate s) a)
annotate i =
  ReaderT $ \e ->
    StateT $ \s ->
      let r = runScript s e i
          ast = fmap fst r
          s' = iter copoint $ fmap snd r
      in return (ast, s')

-- | Abstract away all plumbing needed to make source
sourced :: String -> URI -> FilePath
        -> Script Actions () -> (FilePath -> IO ()) -> Script Sources ()
sourced ty url path inner update = Script $ do
  rfp <- view app
  tok <- use token
  let df = constructDestinationFilepath rfp url path
  local (set sourcePath df . set sourceURL url) $ do
    order .= 0
    maxOrder .= size inner
    p <- view profileName
    profiles . contains p .= True
    ast <- annotate inner
    lift . liftF $ TS (AS { asToken = tok, asProfile = p }) (Source ty url df update) ast ()
    token += 1

-- | 'Actions' scope script size (in actual actions)
size :: Script Actions a -> Int
size = (`execState` 0) . go . evalScript def def
 where
  go :: Free (Term Annotate Actions) a -> State Int ()
  go (Free c@(TA {})) = id %= succ >> go (copoint c)
  go (Free c@(TM {})) = go (copoint c)
  go (Pure _) = return ()

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> Action) -> Script Actions ()
actioned f = Script $ do
  rfp <- view app
  sfp <- view sourcePath
  url <- view sourceURL
  o <- order <+= 1
  mo <- use maxOrder
  lift . liftF $ TA (AA { aaURI = url, aaOrder = o, aaMaxOrder = mo }) (f rfp sfp) ()

-- | Construct destination 'FilePath'
--
-- >>> constructDestinationFilepath "" "" ""
-- ""
--
-- >>> constructDestinationFilepath "/root" "from" "to"
-- "/root/to"
--
-- >>> constructDestinationFilepath "/root" "from" "/to"
-- "/to"
--
-- >>> constructDestinationFilepath "/root" "from" "to/"
-- "/root/to/from"
--
-- >>> constructDestinationFilepath "/root" "from" "/to/"
-- "/to/from"
constructDestinationFilepath :: FilePath -> FilePath -> FilePath -> FilePath
constructDestinationFilepath root source destination =
  execState ?? root $ do
    id </>= destination
    when ("/" `isSuffixOf` destination) $
      id </>= (source^.filename)
