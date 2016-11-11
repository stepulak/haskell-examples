sumFold :: Num a => [a] -> a
sumFold = foldr (\x y -> x + y) 0

productFold :: Num a => [a] -> a
productFold = foldr (*) 1

orFold :: [Bool] -> Bool
orFold = foldr (||) False

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

maximumFold :: (Ord a) => [a] -> Maybe a
maximumFold [] = Nothing
maximumFold (x:xs) = Just (foldr (max) x xs)

minimumFold :: (Ord a) => [a] -> Maybe a
minimumFold [] = Nothing
minimumFold (x:xs) = Just (foldr (min) x xs)

lengthFold :: [a] -> Int
lengthFold = foldr (\_ y -> y + 1) 0

maxmin_ :: (Ord a) => a -> (a, a) -> (a, a)
maxmin_ v (mi,ma)
     | v < mi = (v, ma)
     | v > ma = (mi, v)
     | otherwise = (mi, ma)

maxminFold :: (Ord a) => [a] -> Maybe (a, a)
maxminFold [] = Nothing
maxminFold (x:xs) = Just (foldr (maxmin_) (x, x) xs) 

composeFold :: [a -> a] -> a -> a
composeFold fs n = foldr (\f x -> f x) n fs

idFold :: [a] -> [a]
idFold = foldr (:) []

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

listifyFold :: [a] -> [[a]]
listifyFold = foldr (\x y -> [x] : y) []

nullFold :: [a] -> Bool
nullFold = foldr (\_ _ -> False) True

headFold :: [a] -> Maybe a
headFold [] = Nothing
headFold (x:xs) = Just (foldr (\_ y -> y) x xs)

lastFold :: [a] -> Maybe a
lastFold [] = Nothing
lastFold (x:xs) = Just (foldl (\_ y -> y) x xs)

reverseFold :: [a] -> [a]
reverseFold = foldl (flip (:)) []

suffixFold :: [a] -> [[a]]
suffixFold s = foldr (\x y -> (x : (head y)) : y) [[]] s

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x y -> f x : y) []

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f = foldr (\x y -> if f x then x : y else y) []

oddEvenAppend :: (Integral a) => a -> ([a], [a]) -> ([a], [a])
oddEvenAppend v (od, ev)
     | odd v = (v : od, ev)
     | otherwise = (od, v : ev)

oddEvenFold :: (Integral a) => [a] -> ([a], [a])
oddEvenFold = foldr (oddEvenAppend) ([], [])

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f = foldr (\x y -> if f x then x : y else []) []

dropWhileFold :: (a -> Bool) -> [a] -> [a]
dropWhileFold f = foldl (\y x -> if f x && null y then [] else y ++ [x]) []
 
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f n = (foldr (flip f) n) . reverse 
