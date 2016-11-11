-- TREE IMPLEMENTATION FROM IB015 
data BinTree a = Node a (BinTree a) (BinTree a)
               | Empty
               deriving (Eq, Show)
               
treeFold :: (a -> b -> b -> b) -> b -> BinTree a -> b
treeFold n e (Node v l r) = n v (treeFold n e l)
                                (treeFold n e r)
treeFold n e Empty = e
-- END

treeSum :: Num a => BinTree a -> a
treeSum = treeFold (\v l r -> v + l + r) 0

treeProduct :: Num a => BinTree a -> a
treeProduct = treeFold (\v l r -> v * l * r) 1

treeOr :: BinTree Bool -> Bool
treeOr = treeFold (\v l r -> v || l || r) False

treeAnd :: BinTree Bool -> Bool
treeAnd = treeFold (\v l r -> v && l && r) True

treeSize :: BinTree a -> Int
treeSize = treeFold (\v l r -> 1 + l + r) 0

treeHeight :: BinTree a -> Int
treeHeight = treeFold (\_ l r -> 1 + max l r) 0

treeList :: BinTree a -> [a]
treeList = treeFold (\v l r -> l ++ [v] ++ r) []

treeConcat :: BinTree [a] -> [a]
treeConcat = treeFold (\v l r -> l ++ v ++ r) []

treeMax :: (Ord a, Bounded a) => BinTree a -> Maybe a
treeMax Empty = Nothing
treeMax tree = Just (treeFold (\v l r -> max v $ max l r) minBound tree)

treeFlip :: BinTree a -> BinTree a
treeFlip = treeFold (\v l r -> Node v r l) Empty

treeId :: BinTree a -> BinTree a
treeId = treeFold (Node) Empty

rightMostBranch :: BinTree a -> [a]
rightMostBranch = treeFold (\v l r -> [v] ++ r) []

leftMostBranch :: BinTree a -> [a]
leftMostBranch = treeFold (\v l r -> l ++ [v]) []

treeRoot :: BinTree a -> a
treeRoot = treeFold (\v _ _ -> v) undefined

treeNull :: BinTree a -> Bool
treeNull = treeFold (\_ _ _ -> False) True

leavesCount :: BinTree a -> Int
leavesCount = treeFold (\_ l r -> if l == 0 && r == 0 then 1 else l + r) 0

leavesList :: BinTree a -> [a]
leavesList = treeFold (\v l r -> if null l && null r then [v] else l ++ r) []

treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f = treeFold (\v l r -> Node (f v) l r) Empty

treeAny :: (a -> Bool) -> BinTree a -> Bool
treeAny f = treeFold (\v l r -> f v || l || r) False

treePair :: Eq a => BinTree (a, a) -> Bool
treePair = treeFold (\(a,b) l r -> a == b && l && r) True

subtreeSums :: Num a => BinTree a -> BinTree a
subtreeSums tree = treeFold (\v l r -> (Node (sum' v l r) l r)) Empty tree
    where
     sum' v Empty (Node rv _ _) = v + rv
     sum' v (Node lv _ _) Empty = v + lv
     sum' v (Node lv _ _) (Node rv _ _) = v + lv + rv
     sum' v Empty Empty = v

findPredicates :: a -> BinTree (Int, a -> Bool) -> [Int]
findPredicates n = treeFold (\(a, f) l r -> (if f n then [a] else []) ++ l ++ r) []