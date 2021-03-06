median :: [a] -> Maybe a
median [] = Nothing
median [x] = Just (x)
median [x,y] = Just (x)
median s = median $ (tail . init) s

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiouyAEIOUY"

filterVowels :: String -> String
filterVowels = filter (isVowel)

isLower :: Char -> Bool
isLower x = x `elem` ['a'..'z']

isUpper :: Char -> Bool
isUpper x = x `elem` ['A'..'Z']

toUpper :: Char -> Char
toUpper x
    | isLower x = (toEnum $ fromEnum x + (fromEnum 'A' - fromEnum 'a')) :: Char
    | otherwise = x

toUpperStr :: String -> String
toUpperStr = map (toUpper)

toLower :: Char -> Char
toLower x
    | isUpper x = (toEnum $ fromEnum x - (fromEnum 'A' - fromEnum 'a')) :: Char
    | otherwise = x
    
toLowerStr :: String -> String
toLowerStr = map (toLower)

takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x:s)
    | n <= 0 = []
    | otherwise = x : takeN (n-1) s
    
getPrimes :: [Int]
getPrimes = filter [2..]
    where
     filter (x:s) = x : [y | y <- filter s, y `mod` x /= 0]

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:s) = qsort [y | y <- s, y <= x] ++ [x] ++ qsort [y | y <- s, y > x]

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort s = foldr (insert) [] s
    where
     insert x [] = [x]
     insert x (y:s) = if x > y then y : insert x s else x : y : s

asciiToString :: [Int] -> String
asciiToString [] = []
asciiToString (x:xs)
     | fromEnum 'a' <= x && x <= fromEnum 'z' = (toEnum x) : asciiToString xs
     | fromEnum 'A' <= x && x <= fromEnum 'Z' = (toEnum x) : asciiToString xs
     | otherwise = asciiToString xs

pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = [(a, b, c) | c <- [1..n], a <- [1..c], b <- [1..c], c^2 == a^2 + b^2]

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [x] = f acc x
foldl' f acc (x:y:s) = foldl' (f) (f acc x) (y:s)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:s) (y:t) = (x,y) : (zip' s t)
