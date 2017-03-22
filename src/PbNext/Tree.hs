{-# LANGUAGE InstanceSigs #-}

module PbNext.Tree
    ( Tree(..)
    , Forest
    ) where

data Tree node
    = Leaf node
    | Internal [Tree node] node
    deriving (Show, Eq)

type Forest node = [Tree node]

instance Foldable Tree where
    foldMap :: (Monoid m) => (node -> m) -> Tree node -> m
    foldMap f (Leaf node) = f node `mappend` mempty
    foldMap f (Internal children node)
        = f node `mappend` (mconcat $ map (foldMap f) children)