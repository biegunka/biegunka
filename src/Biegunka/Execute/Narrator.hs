{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Reader (MonadReader, MonadIO, liftIO)
import Data.Proxy
import Data.Reflection

import Biegunka.Execute.State


type Narrative = Chan Statement


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


narrate :: (Reifies s (Narrative, EE), MonadIO m) => Proxy s -> Statement -> m ()
narrate p s = do
  let ch = view _1 (reflect p)
  liftIO $ writeChan ch s
