{-# LANGUAGE ExistentialQuantification #-}
module Control.Biegunka.Templates
  ( TemplateSystem(..)
  , Templates(..)
  ) where

import Data.Text (Text)


-- | Templating implementations
class TemplateSystem t where
  templating
    :: t
    -> Text -- ^ Incoming text
    -> Text -- ^ Generated outcome

-- | Wraps templating system, hiding implementation
data Templates = forall t. TemplateSystem t => Templates t
