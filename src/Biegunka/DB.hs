{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Biegunka.DB
  ( Biegunka
  , load, save, construct
  , filepaths, sources
  ) where

import Control.Applicative ((<$>), empty)
import Control.Exception (Exception, SomeException, handle, throw)
import Control.Monad ((<=<))
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)

import           Control.Lens hiding ((.=), (<.>))
import qualified Control.Lens as CL
import           Control.Monad.Free (Free)
import           Control.Monad.State (State, execState)
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           System.Directory (getHomeDirectory, removeFile)
import           System.FilePath ((</>), (<.>))

import Biegunka.Language


newtype Biegunka = Biegunka
  { unBiegunka :: Map String (Map FilePath (Set FilePath)) } deriving (Show, Eq, Monoid)


newtype Repos = Repos (Map FilePath (Set FilePath))


data AesonFailedToDecode = AesonFailedToDecode deriving (Typeable, Show)


data Construct = Construct
  { _profile :: String
  , _source :: FilePath
  , _biegunka :: Map String (Map FilePath (Set FilePath))
  }


makeLenses ''Construct


instance Exception AesonFailedToDecode


instance FromJSON Repos where
  parseJSON (Object o) = Repos . M.fromList <$> (mapM repo =<< o .: "repos")
   where
    repo r = do
      n <- r .: "path"
      fs <- r .: "files"
      return (n, S.fromList fs)
  parseJSON _ = empty


instance ToJSON Repos where
  toJSON (Repos α) =
    object ["repos" .= map repoToJSON (M.toList α)]
   where
    repoToJSON (k, v) = object ["path" .= k, "files" .= S.toList v]


load :: Free (Command l s) c -> IO Biegunka
load = fmap (Biegunka . M.fromList) . mapM readProfile . catMaybes . foldie (:) [] f
 where
  readProfile k = do
    h <- getHomeDirectory
    handle (\(_ :: SomeException) -> return mempty) $ do
      t <- B.readFile (h </> ".biegunka" <.> k)
      case decode (fromStrict t) of
        Just (Repos p) -> return (k, p)
        Nothing -> throw AesonFailedToDecode

  f (P name _ _) = Just name
  f _ = Nothing


save :: Biegunka -> IO ()
save (Biegunka x) = getHomeDirectory >>= \hd ->
  traverseWithKey_ (\k a ->
    let bname = (hd </> ".biegunka" <.> k) in
      if M.null a
        then handle (\(_ :: SomeException) -> return ()) $
          removeFile bname
        else BL.writeFile bname (encode (Repos a))) x
 where
  traverseWithKey_ f m = itraverse f m >> return ()


filepaths :: Biegunka -> [FilePath]
filepaths = S.toList <=< M.elems <=< M.elems . unBiegunka


sources :: Biegunka -> [FilePath]
sources = M.keys <=< M.elems . unBiegunka


fromStrict :: B.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return


construct :: Free (Command l ()) a -> Biegunka
construct cs = Biegunka $
  execState (foldieM_ g cs) Construct { _profile = mempty, _source = mempty, _biegunka = mempty } ^. biegunka


g :: Command l () (Free (Command l ()) a) -> State Construct ()
g (P name _ _) = do
  profile CL..= name
  biegunka . at name ?= mempty
g (S _ s _ _ _) = do
  p <- use profile
  source CL..= s
  biegunka . at p . traverse . at s ?= mempty
g (F a _) = do
  p <- use profile
  s <- use source
  biegunka . at p . traverse . at s . traverse <>= h a
 where
  h (Message _) = mempty
  h (RegisterAt _ dst) = S.singleton dst
  h (Link _ dst) = S.singleton dst
  h (Copy _ dst) = S.singleton dst
  h (Template _ dst _) = S.singleton dst
  h (Shell {}) = mempty
g (W _ _) = return ()
