{-# LANGUAGE ExistentialQuantification #-}
-- | Generic TemplateSystem interface
module Control.Biegunka.Templates
  ( TemplateSystem(..)
  , Templates(..)
  ) where

import Data.Text (Text)


-- | A templating system implementation.
class TemplateSystem t where
  templating :: t -> Text -> Text

-- | Wrap a templating system, hiding the implementation.
data Templates = forall t. TemplateSystem t => Templates t
