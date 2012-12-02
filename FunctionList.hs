{-# LANGUAGE TupleSections #-}

module FunctionList where

type Index = Integer
type List a = Index -> Maybe a

emptyF :: List a
emptyF _ = Nothing

(-:-) :: a -> List a -> List a
(x -:- xs) 0 = Just x
(x -:- xs) i = xs (i-1)
infixr 5 -:-

viewF :: List a -> Maybe (a, List a)
viewF xs = (, xs') `fmap` xs 0
    where
        xs' i = xs (i+1)

headF :: List a -> Maybe a
headF = fmap fst . viewF

tailF :: List a -> Maybe (List a)
tailF = fmap snd . viewF

nullF :: List a -> Bool
nullF (viewF -> Nothing) = True
nullF _                  = False

foldrF :: (a -> b -> b) -> b -> List a -> b
foldrF f z = maybe z f' . viewF
    where
        f' (x, xs) = f x (foldrF f z xs)

foldlF :: (b -> a -> b) -> b -> List a -> b
foldlF f = go
    where
        go y (viewF -> Just (x, xs)) = go (f y x) xs
        go y _                       = y

fromF :: List a -> [a]
fromF = foldrF (:) []

toF :: [a] -> List a
toF = foldr (-:-) emptyF

(-+-) :: List a -> List a -> List a
xs -+- ys = foldrF (-:-) ys xs

mapF :: (a -> b) -> List a -> List b
mapF f = foldrF f' emptyF
    where
        f' x ys = f x -:- ys

filterF :: (a -> Bool) -> List a -> List a
filterF p = foldrF p' emptyF
    where
        p' x ys
            | p x       = x -:- ys
            | otherwise = ys

reverseF :: List a -> List a
reverseF = foldlF (flip (-:-)) emptyF