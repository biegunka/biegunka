{-# LANGUAGE ExistentialQuantification #-}
-- | HStringTemplate support as TemplateSystem
module Control.Biegunka.Templates.HStringTemplate
  ( HStringTemplate
  , hStringTemplate
  ) where

import qualified Data.Text as T
import           Text.StringTemplate (ToSElem, newSTMP, render, setAttribute)
import           Text.StringTemplate.GenericStandard ()

import Control.Biegunka.Templates

-- | HStringTemplate templates
data HStringTemplate = forall t. ToSElem t => HStringTemplate t

instance TemplateSystem HStringTemplate where
  templating (HStringTemplate b) = render . setAttribute "template" b . newSTMP . T.unpack
  {-# INLINE templating #-}

-- | Use any stuff with 'ToSElem' instance as templates
hStringTemplate :: ToSElem a => a -> Templates
hStringTemplate = Templates . HStringTemplate
{-# INLINE hStringTemplate #-}
