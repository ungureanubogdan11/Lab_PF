import Data.List (sort)

sum_pat_imp :: [Int] -> Int
sum_pat_imp l = foldr (+) 0 (map (^2) (filter odd l))

verif_true :: [Bool] -> Bool
verif_true = foldr (&&) True

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l = verif_true (map f l)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr (||) False (map f l)

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr ((:) . (f)) [] 

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr ((\x acc -> if f x then x : acc else acc)) []

listToInt :: [Integer] -> Integer
listToInt = foldl (\acc x -> (acc * 10 + x)) 0

rmChar :: Char -> String -> String
rmChar c s = filter (/= c) s

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (c : rest) s = rmCharsRec rest (rmChar c s)

rmCharsFold :: String -> String -> String
rmCharsFold s1 s2 = foldr rmChar s2 s1

myReverse :: [Int] -> [Int]
myReverse = foldr (\a b -> b ++ [a]) []

myElem :: Int -> [Int] -> Bool
myElem x l = anyVerifies (== x) l

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr (\x acc -> ((fst x):(fst acc), (snd x) : (snd acc))) ([], [])

union :: [Int] -> [Int] -> [Int]
union a b = sort (foldr (\x acc -> if elem x a then acc else (x:acc)) a b)

intersect :: [Int] -> [Int] -> [Int]
intersect a b = sort (foldr (\x acc -> if elem x b then (x:acc) else acc) [] a)

permutations :: [a] -> [[a]]
permutations = foldr (\x -> concatMap (\ys -> [take i ys ++ [x] ++ drop i ys | i <- [0 .. length ys]])) [[]]