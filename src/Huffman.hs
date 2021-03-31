{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
module Huffman where

import Data.List
import qualified Data.Map.Strict as MS
import GHC.Generics
import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Binary
import Control.Applicative
import Data.Foldable
import Data.Monoid((<>))

-- =====================================================================----------
-- ===================================================================== structure
data HTree a = HLeaf a
               | HNode (HTree a) (HTree a)
               deriving (Show, Eq, Generic) --Generic
makeHT :: a -> HTree a
makeHT = HLeaf

merge :: HTree a -> HTree a -> HTree a
merge = HNode
-- -------------------------------------------------------------------------
data Weight a = WPair { _Weight:: Int, _Item :: a} deriving (Show, Functor)
instance Eq (Weight a) where
     WPair w1 _ == WPair w2 _ = w1 == w2
instance Ord (Weight a) where
     compare (WPair w1 _) (WPair w2 _) = compare w1 w2

type WeightedHT a = Weight (HTree a)-- HTree contains some weight

mergeWHT :: WeightedHT a -> WeightedHT a -> WeightedHT a
mergeWHT (WPair w1 ht1) (WPair w2 ht2) = WPair (w1 + w2) (merge ht1 ht2)


-- ===============================================================------------------------------
-- ===============================================================building Huffman encoding tree
type FreqTable a = MS.Map a Int

freqList :: Ord a => [a] -> FreqTable a
freqList = foldr f MS.empty 
     where
          f x m = MS.insertWith (+) x 1 m

------building tree
buildTree :: State (PQueue (WeightedHT a)) (Maybe (HTree a))
buildTree = do
    t1' <- state popPQ
    case t1' of
      Nothing ->
        return Nothing
      Just t1 -> do
        t2' <- state popPQ
        case t2' of
          Nothing  ->
            return (Just (_Item t1))
          Just t2 -> do
            let combined = mergeWHT t1 t2
            modify (insertPQ combined)
            buildTree
runBuildTree :: Ord a => [a] -> (Maybe (HTree a))
runBuildTree xs = evalState (listQueueState xs >> buildTree) emptyPQ
-------------------------------------------------------------------------------
listQueueState :: Ord a => [a] -> State (PQueue (WeightedHT a)) ()
listQueueState xs = MS.traverseWithKey addNode (freqList xs) >> return ()
  where
    addNode :: a -> Int -> State (PQueue (WeightedHT a)) ()
    addNode x i = modify (insertPQ (WPair i (makeHT x)))

listQueueStateTable :: Ord a => FreqTable a -> State (PQueue (WeightedHT a)) ()
listQueueStateTable tab = void $ MS.traverseWithKey addNode tab
  where
    addNode :: a -> Int -> State (PQueue (WeightedHT a)) ()
    addNode x i = modify (insertPQ (WPair i (makeHT x)))

-- ===============================================================================----------------
-- =============================================================================== binary instance
instance Binary a => Binary (HTree a) where
    put = putHT
    get = getHT

putHT :: Binary a => HTree a -> Put
putHT (HLeaf x) = do
    Data.Binary.put True
    Data.Binary.put x
putHT (HNode ht1 ht2) = do
    Data.Binary.put False
    Data.Binary.put ht1
    Data.Binary.put ht2

getHT :: Binary a => Get (HTree a)
getHT = do
    isLeaf <- Data.Binary.get
    if isLeaf
        then HLeaf <$> Data.Binary.get
        else HNode <$> Data.Binary.get <*> Data.Binary.get

-- ===========================================================================================-----------------
-- =========================================================================================== Heap and PQueue

data Heap a = HeEmpty | HeNode a (Heap a) (Heap a) deriving (Show, Eq, Foldable)

makeHeap :: a -> Heap a
makeHeap x = HeNode x HeEmpty HeEmpty

popHeap :: Ord a => Heap a -> (Maybe a, Heap a)
popHeap HeEmpty = (Nothing, HeEmpty)
popHeap (HeNode r h1 h2) = (Just r , mergeHeap h1 h2)

mergeHeap :: Ord a => Heap a -> Heap a -> Heap a
mergeHeap HeEmpty h = h
mergeHeap h HeEmpty = h
mergeHeap (HeNode xA lA rA) (HeNode xB lB rB)
    | xA < xB = HeNode xA (mergeHeap rA (HeNode xB lB rB)) lA
    | otherwise = HeNode xB (mergeHeap rB(HeNode xA lA rA)) lB
-- =========================================== Priority Queue interface
newtype PQueue a = PQ (Heap a) deriving Show

emptyPQ :: PQueue a
emptyPQ = PQ HeEmpty

insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeHeap h (makeHeap x))

popPQ :: Ord a => PQueue a -> (Maybe a, PQueue a)
popPQ (PQ h) = (res, PQ h')
  where
    (res, h') = popHeap h

-- ===========================================----------
-- =========================================== HuffTable
data Direction = DLeft | DRight deriving (Show, Eq, Generic) -- 0/1
type Encoding = [Direction]

htTable :: Ord a => HTree a -> MS.Map a Encoding
htTable ht = go ht []
    where
        go (HLeaf x) enc = x `MS.singleton` reverse enc
        go (HNode ht1 ht2) enc = go ht1 (DLeft : enc) <> --full combination
                                 go ht2 (DRight : enc)
