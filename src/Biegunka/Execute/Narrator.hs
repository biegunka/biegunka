module Biegunka.Execute.Narrator
  ( -- * Narrator settings
    Volubility(..), Statement(..)
    -- * Narrator control functions
  , Narrative, narrator, narrate
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)

import Control.Lens
import Control.Monad.State (StateT, liftIO)


type Narrative = Chan Statement

-- | Narrator volubility: how verbose are her reports?
data Volubility =
    Talkative -- ^ Says everything you told her
  | Casual    -- ^ Casual narrator
  | Taciturn  -- ^ Doesn't say anything
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Statement thoroughness
data Statement =
    Thorough { text :: String } -- ^ Highly verbose statement with lots of details
  | Typical  { text :: String } -- ^ Typical report with minimum information
    deriving (Show, Read, Eq, Ord)


-- | Start narrator in separate thread awaiting reports
-- to tell to biegunka user
narrator :: Volubility -> IO Narrative
narrator v = do
  ch <- newChan
  forkIO . forever $ readChan ch >>= state v
  return ch


-- | Statement processing routine, describes narrator's behaviour
-- depending on volubility setting and statement thoroughness
state :: Volubility -> Statement -> IO ()
state Talkative m         = putStrLn (text m)
state Casual (Typical  m) = putStrLn m
state Casual (Thorough _) = return ()
state Taciturn _          = return ()


narrate :: Statement -> StateT (Narrative, a) IO ()
narrate s = do
  ch <- use _1
  liftIO $ writeChan ch s
