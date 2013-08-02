{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Configuration script machinery
module Control.Biegunka.Script
  ( Script(..), Annotations, Annotate(..)
  , script, annotate, URI, sourced, actioned, constructDestinationFilepath
  , token, app, profiles, profileName, source, sourceURL, order
  , runScript, runScript', evalScript
  ) where

import Control.Applicative (Applicative(..), (<$))
import Control.Monad (when)
import Data.List (isSuffixOf)

import           Control.Lens hiding (Action)
import           Control.Monad.Free (Free(..), iter, liftF)
import           Control.Monad.State (MonadState(..), StateT(..), State, execState, lift, state)
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
  { unScript :: StateT Annotations (Free (Term Annotate s)) a
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
runScript :: Annotations -> Script s a -> Free (Term Annotate s) (a, Annotations)
runScript as (Script s) = runStateT s as
{-# INLINE runScript #-}

-- | Get DSL and resulting state from 'Script'
runScript' :: Annotations -> Script s a -> (Free (Term Annotate s) a, Annotations)
runScript' as (Script s) =
  let ast      = runStateT s as
      (a, as') = iter copoint ast
  in (a <$ ast, as')
{-# INLINE runScript' #-}

-- | Get DSL from 'Script'
evalScript :: Annotations -> Script s a -> Free (Term Annotate s) a
evalScript = (fmap fst .) . runScript
{-# INLINE evalScript #-}

-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String

-- | Script construction state
data Annotations = Annotations
  { _token :: Int           -- ^ Unique term token
  , _app :: FilePath        -- ^ Biegunka root filepath
  , _profiles :: Set String -- ^ Profile name
  , _profileName :: String  -- ^ Profile name
  , _source :: FilePath     -- ^ Source root filepath
  , _sourceURL :: URI       -- ^ Current source url
  , _order :: Int           -- ^ Current action order
  , _maxOrder :: Int        -- ^ Maximum action order in current source
  } deriving (Show, Read)

instance Default Annotations where
  def = Annotations
    { _token = 0
    , _app = ""
    , _profiles = S.empty
    , _profileName = ""
    , _source = ""
    , _sourceURL = ""
    , _order = 0
    , _maxOrder = 0
    }

makeLensesWith ?? ''Annotations $ defaultRules & generateSignatures .~ False

-- | Unique token for each 'TP'/'TS'
token :: Lens' Annotations Int

-- | Biegunka filepath root
app :: Lens' Annotations FilePath

-- | All profiles encountered so far
profiles :: Lens' Annotations (Set String)

-- | Current profile name
profileName :: Lens' Annotations String

-- | Current source filepath
source :: Lens' Annotations FilePath

-- | Current source url
sourceURL :: Lens' Annotations String

-- | Current action order
order :: Lens' Annotations Int

-- | Maximum action order in current source
maxOrder :: Lens' Annotations Int

-- | Lift DSL term to the 'Script'
script :: Term Annotate s a -> Script s a
script = Script . lift . liftF
{-# INLINE script #-}

-- | Annotate DSL
annotate :: Script s a -> StateT Annotations (Free (Term Annotate t)) (Free (Term Annotate s) a)
annotate i = state $ \s ->
  let r = runScript s i
      ast = fmap fst r
      s' = iter copoint $ fmap snd r
  in (ast, s')

-- | Abstract away all plumbing needed to make source
sourced :: String -> URI -> FilePath
        -> Script Actions () -> (FilePath -> IO ()) -> Script Sources ()
sourced ty url path inner update = Script $ do
  rfp <- use app
  tok <- use token
  let df = constructDestinationFilepath rfp url path
  source .= df
  sourceURL .= url
  order .= 0
  maxOrder .= size inner
  p <- use profileName
  profiles . contains p .= True
  ast <- annotate inner
  lift . liftF $ TS (AS { asToken = tok, asProfile = p }) (Source ty url df update) ast ()
  token += 1

-- | 'Actions' scope script size (in actual actions)
size :: Script Actions a -> Int
size = (`execState` 0) . go . evalScript def
 where
  go :: Free (Term Annotate Actions) a -> State Int ()
  go (Free c@(TA {})) = id %= succ >> go (copoint c)
  go (Free c@(TM {})) = go (copoint c)
  go (Pure _) = return ()

-- | Get 'Actions' scope script from 'FilePath' mangling
actioned :: (FilePath -> FilePath -> Action) -> Script Actions ()
actioned f = Script $ do
  rfp <- use app
  sfp <- use source
  url <- use sourceURL
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
constructDestinationFilepath r s d = execState ?? r $ do
  id </>= d
  when ("/" `isSuffixOf` d) $
    id </>= (s^.filename)
