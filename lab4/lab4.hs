{-
[x^2 |x <- [1..10], x `rem` 3 == 2]
[(x,y) | x <- [1..5], y <- [x..(x+2)]]
[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
[[x..y] | x <- [1..5], y <- [1..5], x < y]
-}
factori :: Int -> [Int]
factori n = [x | x <- [1 .. n], mod n x == 0]

prim :: Int -> Bool
prim 1 = False
prim n = length (factori n) == 2

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2 .. n], prim x]

myzip3 :: [a] -> [a] -> [a] -> [(a, a, a)]
flat (a, (b, c)) = (a, b, c)
myzip3 a b c = [flat x | x <- zip a (zip b c)]

firstEl :: [(a, b)] -> [a]
firstEl list = map fst list

sumList :: [[Int]] -> [Int]
sumList x = map sum x

help_prel2 x = 
    if mod x 2 == 0
        then div x 2
    else
        x * 2
prel2 :: [Int] -> [Int]
prel2 x = map help_prel2 x

apartine :: Char -> [String] -> [String]
apartine c l = filter (elem c) l

impare_patrate :: [Int] -> [Int]
impare_patrate l = filter odd [x * x | x <- l]

poz_impare_patrate :: [Int] -> [Int]
poz_impare_patrate x = map ((^2) . fst) ((filter (odd . snd)) (zip x [1 .. ]))

numaiVocale :: [String] -> [String]
numaiVocale = map (filter (`elem` "aeiouAEIOU"))

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x : xs) = [f x] ++ mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x : xs) = 
    if f x
        then [x] ++ myfilter f xs
    else
        myfilter f xs

list_tuple [(a, b)] = (a, b)
list_list_tuple l = map list_tuple l 

adauga :: Char -> Int -> [(Char, Int)] -> [(Char, Int)]
adauga car poz [] = []
adauga car poz (curr : rest) = 
    if snd curr == poz && fst curr == '-'
        then [(car, snd curr)] ++ adauga car poz rest
    else
        (curr : adauga car poz rest)


step :: Char -> [(Char, Int)] -> [[(Char, Int)]]
step car harta = [adauga car poz harta | poz <- [1 .. 9]]

next :: Char -> [[(Char, Int)]] -> [[(Char, Int)]]
next p cs = concatMap (\x -> step p x) cs



verif :: [(Char, Int)] -> Bool
-- verif verifica daca am 3 el egale pe linie coloana sau diagonala 

check_win :: [[(Char, Int)]] -> [Bool]
check_win [] = []
check_win (head:tail) = 
    verif head ++ (check_win tail)

winner :: Char -> [(Char, Int)] -> Bool
winner car cs = foldr (||) False (check_win (step car cs))

win :: Char -> [(Char, Int)] -> [(Char, Int)]
win car cs = filter (winner car) cs

-- to be continued (or maybe not)