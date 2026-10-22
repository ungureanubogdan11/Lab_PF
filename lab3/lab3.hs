import Data.Char (digitToInt, isDigit)
import Data.List (permutations, sort)




verifL :: [Int] -> Bool
verifL x = 
    mod (length x) 2 == 0

takefinal :: [a] -> Int -> [a]
takefinal x 0 = []
takefinal (head : tail) n = 
    if length (head:tail) == n
        then (head:tail)
    else 
        takefinal tail n

remove :: [a] -> Int -> [a]
remove x n = 
    (take (n - 1) x) ++ (takefinal x (length x - n))

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t
 
myreplicate :: Int -> Int -> [Int]
myreplicate x 0 = []
myreplicate x n = 
    (x : myreplicate x (n - 1))


sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x : y) =
    if mod x 2 == 0
        then sumImp y
    else 
        x + sumImp y


totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x : y) =
    if take 1 x == "A"
        then length x + totalLen y
    else
        totalLen y

nrVocale :: [String] -> Int
nrVocale [] = 0
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9
nrVocale (x : y) =
    if x == reverse x
        then vocale x + nrVocale y 
    else    
        nrVocale y

vocale :: String -> Int
vocale [] = 0
vocale (x : y) = 
    if elem x "aeiou"
        then 1 + vocale y
    else 
        vocale y 


f :: Int -> [Int] -> [Int]
f x [] = []
f x (a : b) = 
    if mod a 2 == 0
        then [a] ++ [x] ++ f x b 
    else    
        [a] ++ f x b

divizori :: Int -> [Int]
divizori x = [a | a <- [1..x], mod x a == 0]

listadiv :: [Int] -> [[Int]]
listadiv l = [divizori x | x <- l]

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]


-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [x | x <- l, a <= x && x <= b]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (x : y) = 
    if a <= x && x <= b
        then [x] ++ inIntervalRec a b y
    else 
        inIntervalRec a b y


pozitiveComp :: [Int] -> Int
pozitiveComp l =  length [x | x <- l, x >= 0]

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x : y) =
    if x > 0
        then 1 + pozitiveRec y
    else 
        pozitiveRec y

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec (x : y) = 
    if even (length y)
        then [x] ++ pozitiiImpareRec y
    else
        pozitiiImpareRec y





multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x : y) =
    if isDigit x
        then digitToInt x * multDigitsRec y
    else
        multDigitsRec y


multDigitsComp :: String -> Int
multDigitsComp l = product [digitToInt x | x <- l, isDigit x]

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

-- bonus

perm :: [a] -> [[a]]
perm l = permutations l


combinari :: Int -> Int -> [[Int]]
combinari x y = [take y a | a <- perm [1..x], take y a == sort (take y a)]

aranjamente :: Int -> Int -> [[Int]]
aranjamente x y = [take y a | a <- perm [1..x]]


-- problema damelor

ok :: Int -> Int -> [(Int, Int)] -> Bool
ok a b [] = True
ok a b ((x, y) : rest) = 
    if a /= x && b /= y && abs (a - x) /= abs (b - y)
        then ok a b rest
    else 
        False


verif :: [(Int, Int)] -> Bool
verif [] = True
verif ((a, b) : c) =
    if ok a b c
        then verif c
    else 
        False

dame :: Int -> Int -> [[(Int, Int)]]
dame n d = take 1 [take d (zip x [1 .. n]) | x <- aranjamente n d, verif (zip x [1..n])]