{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Data.Aeson.Encode.Pretty
  ( EncodingEnv(..), Parentheses(..)
  , encode
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intersperse)
import Data.Monoid ((<>), mappend, mconcat)

import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Aeson.Encode as A (fromValue)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V


data Parentheses = Allman
                 | KnR
                   deriving (Show, Read, Eq, Ord)


data EncodingEnv = EncodingEnv
  { indentationStep ∷ Int
  , parentheses ∷ Parentheses
  } deriving (Show, Read, Eq, Ord)


encode ∷ ToJSON α ⇒ EncodingEnv → α → ByteString
encode env = encodeUtf8 . toLazyText . (\val → runReader (evalStateT (fromValue val) 0) env) . toJSON


fromValue ∷ Value → StateT Int (Reader EncodingEnv) Builder
fromValue (Array v) = fromComplex ("[","]") fromValue (V.toList v)
fromValue (Object m) = fromComplex ("{","}") fromKeyValue (H.toList m)
fromValue v = return $ A.fromValue v


fromComplex ∷ (Builder, Builder) → (a → StateT Int (Reader EncodingEnv) Builder) → [a] → StateT Int (Reader EncodingEnv) Builder
fromComplex (delimL,delimR) _ [] = return $ delimL <> delimR
fromComplex (delimL,delimR) fromItem items =
  do spaces ← fromIndent
     modify succ
     items' ← mconcat . intersperse ",\n" <$>
       mapM (\item → mappend <$> fromIndent <*> fromItem item) items
     modify pred
     asks $ \env → mconcat $ case env of
       EncodingEnv { parentheses = KnR } → [delimL, "\n", items', "\n", spaces, delimR]
       EncodingEnv { parentheses = Allman } → ["\n", spaces, delimL, "\n", items', "\n", spaces, delimR]


fromKeyValue ∷ (Text, Value) → StateT Int (Reader EncodingEnv) Builder
fromKeyValue (k,v) =
  do encoding ← fromValue v
     return $ A.fromValue (toJSON k) <> ": " <> encoding


fromIndent ∷ StateT Int (Reader EncodingEnv) Builder
fromIndent =
  do indent ← get
     asks (\EncodingEnv { indentationStep = step } → mconcat $ replicate (indent * step) " ")
