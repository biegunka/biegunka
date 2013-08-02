{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | Configuration script machinery
--
-- Gets interesting static information from script
-- before passing it to interpreters, which then make use of it
module Control.Biegunka.Script
  ( -- * Script types
    Script(..), Annotate(..)
    -- ** Implementation details
  , AnnotationsState, AnnotationsEnv
    -- * Get annotated script
  , runScript, runScript', evalScript
    -- * Script mangling
  , script, annotate, sourced, actioned, constructDestinationFilepath
    -- * Lenses
  , app, profileName, sourcePath, sourceURL, profiles
  , token, order, sourceReaction, actionReaction, activeUser, maxRetries
    -- ** Misc
  , URI, UserW(..), User(..), React(..), Retry(..), incr
  ) where

import Control.Applicative (Applicative(..), (<$))
import Control.Lens hiding (Action)
import Control.Monad (when)
import Control.Monad.Free (Free(..), iter, liftF)
import Control.Monad.State (StateT(..), State, execState)
import Control.Monad.Reader (ReaderT(..), local)
import Control.Monad.Trans (lift)
import Data.Copointed (copoint)
import Data.Default (Default(..))
import Data.List (isSuffixOf)
import Data.Set (Set)
import Data.String (IsString(..))
import System.FilePath.Lens
import System.Posix.Types (CUid)

import Control.Biegunka.Language


-- | Language 'Term' annotation depending on their 'Scope'
data family Annotate (sc :: Scope) :: *
data instance Annotate Sources = AS
  { asToken :: Int
  , asProfile :: String
  , asUser :: Maybe UserW
  , asMaxRetries :: Retry
  , asReaction :: React
  }
data instance Annotate Actions = AA
  { aaURI :: URI
  , aaOrder :: Int
  , aaMaxOrder :: Int
  , aaUser :: Maybe UserW
  , aaMaxRetries :: Retry
  , aaReaction :: React
  }


-- | Newtype used to provide better error messages
-- for type errors in DSL (for users, mostly)
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

-- | Get annotated DSL and resulting state
runScript
  :: AnnotationsState
  -> AnnotationsEnv
  -> Script s a
  -> Free (Term Annotate s) (a, AnnotationsState)
runScript as ae (Script s) = runStateT (runReaderT s ae) as
{-# INLINE runScript #-}

-- | Get annotated DSL and resulting state
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

-- | Get annotated DSL
evalScript
  :: AnnotationsState
  -> AnnotationsEnv
  -> Script s a
  -> Free (Term Annotate s) a
evalScript = ((fmap fst .) .) . runScript
{-# INLINE evalScript #-}

-- | Lift DSL term to the 'Script'
script :: Term Annotate s a -> Script s a
script = Script . lift . liftF
{-# INLINE script #-}


-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String

-- | User setting modifier
data User u where
  UserID   :: CUid -> User CUid     -- ^ Set user with ID
  Username :: String -> User String -- ^ Set user with username

instance Show (User u)

instance u ~ String => IsString (User u) where
  fromString = Username

-- | Because I can
instance u ~ CUid => Num (User u) where
  UserID a + UserID b = UserID (a + b)
  UserID a * UserID b = UserID (a * b)
  abs (UserID a)      = UserID (abs a)
  signum (UserID a)   = signum (UserID a)
  fromInteger         = UserID . fromInteger

data UserW = forall u. UserW (User u)

deriving instance Show UserW

-- | Failure reaction
data React = Ignorant | Abortive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype Retry = Retry { unRetry :: Int }
    deriving (Show, Read, Eq, Ord)

instance Default Retry where
  def = Retry def

incr :: Retry -> Retry
incr (Retry n) = Retry (succ n)

-- | Script construction state
data AnnotationsState = AState
  { _token :: Int           -- ^ Unique term token
  , _profiles :: Set String -- ^ Profile name
  , _order :: Int           -- ^ Current action order
  , _maxOrder :: Int        -- ^ Maximum action order in current source
  } deriving (Show, Read)

instance Default AnnotationsState where
  def = AState
    { _token = def
    , _profiles = def
    , _order = def
    , _maxOrder = def
    }
  {-# INLINE def #-}

data AnnotationsEnv = AEnv
  { _app         :: FilePath    -- ^ Biegunka root filepath
  , _profileName :: String      -- ^ Profile name
  , _sourcePath  :: FilePath    -- ^ Source root filepath
  , _sourceURL   :: URI         -- ^ Current source url
  , _activeUser  :: Maybe UserW -- ^ Maximum action order in current source
  , _maxRetries  :: Retry       -- ^ Maximum retries count
  , _sourceReaction :: React    -- ^ How to react on source failure
  , _actionReaction :: React    -- ^ How to react on action failure
  }

deriving instance Show AnnotationsEnv

instance Default AnnotationsEnv where
  def = AEnv
    { _app = def
    , _profileName = def
    , _sourcePath = def
    , _sourceURL = def
    , _activeUser = def
    , _maxRetries = Retry 1
    , _sourceReaction = Abortive
    , _actionReaction = Ignorant
    }
  {-# INLINE def #-}


-- * Lenses

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

-- | Current user
activeUser :: Lens' AnnotationsEnv (Maybe UserW)

-- | Maximum retries count
maxRetries :: Lens' AnnotationsEnv Retry

-- | How to react on source failure
sourceReaction :: Lens' AnnotationsEnv React

-- | How to react on action failure
actionReaction :: Lens' AnnotationsEnv React


-- * Script mangling

-- | Annotate DSL
annotate
  :: Script s a
  -> ReaderT AnnotationsEnv
      (StateT AnnotationsState (Free (Term Annotate t))) (Free (Term Annotate s) a)
annotate i =
  ReaderT $ \e ->
    StateT $ \s ->
      let r = runScript s e i
          ast = fmap fst r
          s' = iter copoint $ fmap snd r
      in return (ast, s')

-- | Abstract away all plumbing needed to make source
sourced
  :: String -> URI -> FilePath
  -> Script Actions () -> (FilePath -> IO ()) -> Script Sources ()
sourced ty url path inner update = Script $ do
  rfp <- view app
  tok <- use token
  let df = constructDestinationFilepath rfp url path
  local (set sourcePath df . set sourceURL url) $ do
    order    .= 0
    maxOrder .= size inner

    profile <- view profileName
    profiles . contains profile .= True

    retries <- view maxRetries
    user    <- view activeUser
    source  <- view sourcePath
    ast     <- annotate inner
    react   <- view sourceReaction
    let annotation = AS
          { asToken = tok
          , asProfile = profile
          , asUser = user
          , asMaxRetries = retries
          , asReaction = react
          }
    lift . liftF $
      TS annotation (Source ty url source update) ast ()

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
  rfp     <- view app
  sfp     <- view sourcePath
  url     <- view sourceURL
  o       <- order <+= 1
  mo      <- use maxOrder
  user    <- view activeUser
  retries <- view maxRetries
  react   <- view actionReaction
  let annotation = AA
       { aaURI = url
       , aaOrder = o
       , aaMaxOrder = mo
       , aaUser = user
       , aaMaxRetries = retries
       , aaReaction = react
       }
  lift . liftF $
    TA annotation (f rfp sfp) ()

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
