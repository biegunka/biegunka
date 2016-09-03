{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module implements the Patience Diff algorithm.
--
-- The code here is heavily based on the "patience" package by
-- Keegan McAllister <https://hackage.haskell.org/package/patience>
module Control.Biegunka.Patience
  ( Judgement(..)
  , judgement
  , diff
  , FileDiff
  , Hunk(..)
  , fileDiff
  ) where

import           Control.Applicative
import           Data.Foldable (foldr, toList)
import           Data.Function (on)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List (sortBy, foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import           Prelude hiding (foldr)


-- | Every element in a diff can be either removed, added, or matched
data Judgement a
  = Removed a
  | Added a
  | Matched a
    deriving (Show, Eq, Functor)

-- | Eliminator for 'Judgement's.
judgement :: (a -> b) -> (a -> b) -> (a -> b) -> Judgement a -> b
judgement f _ _ (Removed a) = f a
judgement _ g _ (Added a)   = g a
judgement _ _ h (Matched a) = h a

type FileDiff = [Hunk Text]

fileDiff :: FilePath -> FilePath -> IO [Hunk Text]
fileDiff x y =
  liftA2 (fromDiff .: diff)
         (fmap Text.lines (Text.readFile x))
         (fmap Text.lines (Text.readFile y))

-- | Compute the diff between two lists. A diff is an ordered sequence of "judgements"
-- on whether to remove, to add, or to leave an element alone.
diff :: Ord a => [a] -> [a] -> [Judgement a]
diff = toList .: go `on` Seq.fromList
 where
  -- Match equal elements on the left side of the sequence
  go (Seq.viewl -> x :< xs) (Seq.viewl -> y :< ys)
    | x == y = Matched x <| go xs ys
  -- Match equal elements on the right side of the sequence
  go (Seq.viewr -> xs :> x) (Seq.viewr -> ys :> y)
    | x == y = go xs ys |> Matched x
  -- Compute the longest common subsequence of unique elements, split on its elements,
  -- decide whether they match or not and recurse into subproblems
  go xs ys = case parts (lcs xs ys) xs ys of
    [Diff as bs] -> fmap Removed as >< fmap Added bs
    zs -> loop zs

  loop [] = Seq.empty
  loop (Same x : zs) = loop zs |> Matched x
  loop (Diff as bs : zs) = loop zs >< go as bs


lcs :: Ord a => Seq a -> Seq a -> [(Int, Int)]
lcs = patiently . sortBy (comparing snd) .  Map.elems .: (Map.intersectionWith (,) `on` unique . enumerate)

infixr 8 .:
(.:) :: (a -> b) -> (e -> e' -> a) -> e -> e' -> b
(.:) = (.) . (.)

-- Zip-with-index operation for Seq
enumerate :: Seq a -> Seq (Int, a)
enumerate xs = Seq.zip (Seq.fromList [0 .. Seq.length xs]) xs

-- Compute the mapping of unique elements of the indexed Seq into their index
unique :: Ord k => Seq (v, k) -> Map k v
unique = Map.mapMaybe id . foldr go Map.empty where go (v, k) = Map.insertWith (\_ _ -> Nothing) k (Just v)

data Card a = Card Int a (Maybe (Card a))

type Piles a = IntMap (NonEmpty (Card a))

patiently :: [(Int, a)] -> [(Int, a)]
patiently = extract . populate

populate :: [(Int, a)] -> Piles a
populate = foldl' go IntMap.empty
 where
  go m (x, a) =
    let
      (lt, gt) = IntMap.split x m
      new      = Card x a (NonEmpty.head . fst <$> IntMap.maxView lt)
    in case IntMap.minViewWithKey gt of
      Nothing          -> IntMap.insert x (return new) m
      Just ((k, _), _) -> adjustMove (NonEmpty.cons new) k x m

extract :: Piles t -> [(Int, t)]
extract = maybe [] (walk . NonEmpty.head . fst) . IntMap.maxView
 where
  walk (Card x a c) = (x, a) : maybe [] walk c

adjustMove :: (a -> a) -> Int -> Int -> IntMap a -> IntMap a
adjustMove f k1 k2 m =
  case IntMap.updateLookupWithKey (\_ _ -> Nothing) k1 m of
    (Just v,  m') -> IntMap.insert k2 (f v) m'
    (Nothing, _)  -> m

data Part a
  = Same a
  | Diff (Seq a) (Seq a)
    deriving (Show, Eq)

parts :: [(Int, Int)] -> Seq a -> Seq a -> [Part a]
parts []            (Seq.viewl -> EmptyL) (Seq.viewl -> EmptyL) = []
parts []            xs                    ys                    = [Diff xs ys]
parts ((n, m) : is) xs                    ys                    = Diff xs' ys' : Same x' : parts is xs'' ys''
 where
  (xs'', Seq.viewl -> x' :< xs') = Seq.splitAt n xs
  (ys'', Seq.viewl -> _  :< ys') = Seq.splitAt m ys

data Hunk a = Hunk !Int !Int !Int !Int [Judgement a]
    deriving (Show, Eq, Functor)

fromDiff :: [Judgement a] -> [Hunk a]
fromDiff = boring 1 1
 where
  boring !n !m (Matched _
    : xs@(Matched _ : Matched _ : Matched _ : _)) = boring (n + 1) (m + 1) xs
  boring _  _  [Matched _, Matched _, Matched _] = []
  boring _  _  [Matched _, Matched _] = []
  boring _  _  [Matched _] = []
  boring n  m  xs = exciting n m [] xs

  exciting n m = go 0 0 where
    go !i !j acc (Matched a : Matched b : Matched c : xs@(Matched _ : Matched _ : Matched _ : _)) = let
        i' = i + 3
        j' = j + 3
      in
        Hunk n i' m j' (reverse (Matched c : Matched b : Matched a : acc)) :
        boring (n + i') (m + j') xs
    go i  j  acc (x@(Added _)   : xs) = go i       (j + 1) (x : acc) xs
    go i  j  acc (x@(Removed _) : xs) = go (i + 1) j       (x : acc) xs
    go i  j  acc (x@(Matched _) : xs) = go (i + 1) (j + 1) (x : acc) xs
    go _  _  []  [] = []
    go i  j  acc [] = [Hunk n i m j (reverse acc)]
