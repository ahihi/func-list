{-# LANGUAGE TupleSections #-}

module FunctionList where

type Index = Integer
type List a = Index -> Maybe a

-- The empty list
emptyF :: List a
emptyF _ = Nothing

-- Construction
(-:-) :: a -> List a -> List a
(x -:- xs) 0 = Just x
(x -:- xs) i = xs (i-1)
infixr 5 -:-

-- (x, xs) view
viewF :: List a -> Maybe (a, List a)
viewF xs = (, xs') `fmap` xs 0
    where
        xs' i = xs (i+1)

-- The first element
headF :: List a -> Maybe a
headF = fmap fst . viewF

-- All but the first element
tailF :: List a -> Maybe (List a)
tailF = fmap snd . viewF

-- Null predicate
nullF :: List a -> Bool
nullF (viewF -> Nothing) = True
nullF _                  = False

-- Right fold
foldrF :: (a -> b -> b) -> b -> List a -> b
foldrF f z = maybe z f' . viewF
    where
        f' (x, xs) = f x (foldrF f z xs)

-- Left fold
foldlF :: (b -> a -> b) -> b -> List a -> b
foldlF f = go
    where
        go y (viewF -> Just (x, xs)) = go (f y x) xs
        go y _                       = y

-- Convert to a Prelude list
fromF :: List a -> [a]
fromF = foldrF (:) []

-- Convert from a Prelude list
toF :: [a] -> List a
toF = foldr (-:-) emptyF

-- Concatenation
(-+-) :: List a -> List a -> List a
xs -+- ys = foldrF (-:-) ys xs

-- Map
mapF :: (a -> b) -> List a -> List b
mapF f = foldrF f' emptyF
    where
        f' x ys = f x -:- ys

-- Filter
filterF :: (a -> Bool) -> List a -> List a
filterF p = foldrF p' emptyF
    where
        p' x ys
            | p x       = x -:- ys
            | otherwise = ys

-- Reverse
reverseF :: List a -> List a
reverseF = foldlF (flip (-:-)) emptyF

-- Monadic fold
foldMF :: (Monad m) => (b -> a -> m b) -> b -> List a -> m b
foldMF f = go
    where
        go y (viewF -> Just (x, xs)) = f y x >>= \ y' -> go y' xs
        go y _                       = return y

-- Monadic sequence
sequenceF :: (Monad m) => List (m a) -> m (List a)
sequenceF (viewF -> Just (x, xs)) = x >> sequenceF xs
sequenceF _                       = return emptyF

-- Monadic sequence with no return value
sequenceF_ :: (Monad m) => List (m a) -> m ()
sequenceF_ xs = sequenceF xs >> return ()

-- Monadic map
mapMF :: (Monad m) => (a -> m b) -> List a -> m (List b)
mapMF f (viewF -> Just (x, xs)) = do
    y <- f x
    ys <- mapMF f xs
    return $ y -:- ys
mapMF _ _                       = return emptyF

-- Monadic map with no return value
mapMF_ :: (Monad m) => (a -> m b) -> List a -> m ()
mapMF_ f xs = mapMF f xs >> return ()
