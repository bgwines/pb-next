{-# LANGUAGE InstanceSigs #-}

module Tree
    ( Tree(..)
    ) where

data Tree l i
    = Leaf l
    | Internal [Tree l i] i
    deriving (Show, Eq)

instance Foldable (Tree leaf) where
    foldMap :: (Monoid m) => (a -> m) -> Tree leaf a -> m
    foldMap f (Leaf leaf) = mempty
    foldMap f (Internal children i) = f i `mappend` (mconcat $ map (foldMap f) children)