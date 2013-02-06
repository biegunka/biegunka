{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Biegunka.Execute.Narrator
  ( -- * Narrator settings
    Volubility(..), Statement(..)
    -- * Narrator control functions
  , Narrative, narrator, narrate
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Foldable (for_)

import Control.Lens
import Control.Monad.Reader (MonadIO, liftIO)
import Data.Proxy
import Data.Reflection

import Biegunka.Execute.State


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


narrate :: forall s. (Reifies s EE) => Statement -> Execution s ()
narrate s = E $ liftIO . for_ (view narrative (reflect (Proxy :: Proxy s))) $ flip writeChan s
