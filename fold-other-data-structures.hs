{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module BEncode where

import Data.Foldable (toList)

data RoseTree a = RoseNode a [RoseTree a]
    deriving (Show, Foldable, Functor)
data BiTree a = BiNode a (BiTree a) (BiTree a) | BiLeaf
    deriving (Show, Foldable, Functor)
data LeafTree a = LeafNode (LeafTree a) (LeafTree a) | LeafLeaf a
    deriving (Show, Foldable, Functor)

tree :: RoseTree Int
tree =
    RoseNode 1
        [ RoseNode 2 []
        , RoseNode 3
            [RoseNode 1 []]
        , RoseNode 4
            [RoseNode 2 []]
        ]

foldRoseTree :: (a -> b -> c) -- Node
         -> (c -> b -> b) -- Cons
         -> b             
         -> RoseTree a -> c
foldRoseTree f g z (RoseNode x ts) = f x (foldr (\t acc -> g (foldRoseTree f g z t) acc) z ts)

idRoseTree :: RoseTree a -> RoseTree a
idRoseTree = foldRoseTree RoseNode (:) []

roseTreeToList :: RoseTree a -> [a]
roseTreeToList = foldRoseTree (:) (++) []

mapRoseTree :: (a -> b) -> RoseTree a -> RoseTree b
mapRoseTree f = foldRoseTree (RoseNode . f) (:) []

biTree :: BiTree Int
biTree =
    BiNode 1
        (BiNode 2 BiLeaf BiLeaf)
        (BiNode 3
            (BiNode 4 BiLeaf BiLeaf)
            BiLeaf)

foldBiTree :: (a -> b -> b -> b) -> b -> BiTree a -> b
foldBiTree f z (BiNode a l r) = f a (foldBiTree f z l) (foldBiTree f z r)
foldBiTree _ z BiLeaf         = z

biTreeToList :: BiTree a -> [a]
biTreeToList = foldBiTree (\a l r -> a : l ++ r) []

mapBiTree :: (a -> b) -> BiTree a -> BiTree b
mapBiTree f = foldBiTree (BiNode . f) BiLeaf

leafTree :: LeafTree Int
leafTree =
    LeafNode
        (LeafLeaf 1)
        (LeafNode
            (LeafLeaf 2)
            (LeafLeaf 3))

foldLeafTree :: (b -> b -> b) -> (a -> b) -> LeafTree a -> b
foldLeafTree _ g (LeafLeaf x)   = g x
foldLeafTree f g (LeafNode l r) = f (foldLeafTree f g l) (foldLeafTree f g r)

leafTreeToList :: LeafTree a -> [a]
leafTreeToList = foldLeafTree (++) (:[])

mapLeafTree :: (a -> b) -> LeafTree a -> LeafTree b
mapLeafTree f = foldLeafTree LeafNode (LeafLeaf . f)

infTree :: RoseTree Int 
infTree = RoseNode 1 [infTree]