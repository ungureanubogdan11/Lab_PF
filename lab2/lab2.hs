import Data.List

myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

--maxim :: Ord a => a -> a -> a
maxim x y = if (x > y)
               then x
          else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)
             
maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = 
    if (x >= y && x >= z)
        then x
    else if (y >= z)
        then y
    else 
        z

maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 w x y z = 
    let 
        aux = maxim3 x y z
    in 
        maxim aux w

test4 :: Integer -> Integer -> Integer -> Integer -> Bool
test4 w x y z =
    let 
        aux = maxim4 w x y z
    in
        (aux >= w && aux >= x && aux >= y && aux >= z)
            

sum_patrate :: Integer -> Integer -> Integer
sum_patrate x y =
    x * x + y * y

paritate :: Integer -> String
paritate x =
    if (mod x 2 == 0)
        then "par"
    else
        "impar"

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = 
    factorial (x - 1) * x

mare_dublu :: Integer -> Integer -> Bool
mare_dublu x y = x > 2 * y

max_el :: Ord a => [a] -> a
max_el [x] = x
max_el (head : tail) = 
    maxim head (max_el tail)

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x =
    a * x * x + b * x + c


eeny :: Integer -> String
eeny x = 
    if paritate x == "par"
        then "eeny"
    else 
        "meeny"

fizzbuzz :: Integer -> String
fizzbuzz x = 
    if (mod x 3 == 0 && mod x 5 == 0)
        then "FizzBuzz"
    else if(mod x 3 == 0)
        then "Fizz"
    else if(mod x 5 == 0)
        then "Buzz"
    else
        ""

fizzbuzz2 :: Integer -> String
fizzbuzz2 x
    | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
    | mod x 3 == 0 = "Fizz"
    | mod x 5 == 0 = "Buzz"
    | otherwise = ""


fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacci :: Integer -> Integer
tribonacci x = 
    if x <= 2
        then 1
    else if x == 3
        then 2
    else
        tribonacci (x - 1) + tribonacci (x - 2) + tribonacci (x - 3)

tribonacci2 :: Integer -> Integer
tribonacci2 1 = 1
tribonacci2 2 = 1
tribonacci2 3 = 2
tribonacci2 x =
    tribonacci2 (x - 1) + tribonacci2 (x - 2) + tribonacci2 (x - 3)


binomial :: Integer -> Integer -> Integer
binomial a 0 = 1
binomial 0 b = 0
binomial a b = 
    (binomial (a - 1) b) + (binomial (a - 1) (b - 1))


sumN :: Integer -> Integer
sumN 1 = 1
sumN x = x + sumN (x - 1)

putere :: Integer -> Integer -> Integer
putere x 0 = 1
putere x y =
    x * putere x (y - 1)

inmultire :: Integer -> Integer -> Integer
inmultire 0 x = 0
inmultire x 0 = 0
inmultire x 1 = x
inmultire x y =
    x + inmultire x (y - 1)

cifre :: Integer -> Integer
cifre 0 = 0
cifre x = 
    mod x 10 + cifre (div x 10)

nrcifre :: Integer -> Integer
nrcifre x = 
    if x < 10
        then 1
    else
        1 + nrcifre (div x 10)

prod_pare :: Integer -> Integer
prod_pare x = 
    if x < 10 && mod x 2 == 0
        then x
    else if x < 10 && mod x 2 == 1
        then 1
    else if mod x 2 == 0
        then mod x 10 * prod_pare (div x 10)
    else    
        prod_pare(div x 10)

oglindit :: Integer -> Integer
oglindit x = 
    if x < 10
        then x
    else 
        (mod x 10) * (10 ^ (nrcifre x - 1)) + oglindit (div x 10)
    

aux :: Integer -> Integer -> Bool
aux 1 x = True
aux 2 x = False
aux x y = 
    if y > 1
        then (mod x y == 0) || aux x (y - 1)
    else 
        False 

prim :: Integer -> Bool
prim x = not (aux x (x - 1))


apcifra :: Integer -> Integer -> Integer
apcifra x c = 
    if x < 10 && x == c
        then 1
    else if x < 10 && x /= c
        then 0
    else if mod x 10 == c
        then 1 + apcifra (div x 10) c
    else 
        apcifra (div x 10) c

sterge_cifra :: Integer -> Integer -> Integer
sterge_cifra 0 p = 0
sterge_cifra x p =
    if (nrcifre x) == p
        then sterge_cifra (div x 10) p
    else 
        (sterge_cifra (div x 10) p) * 10 + mod x 10 



sterge_cifra_x :: Integer -> Integer -> Integer
sterge_cifra_x 0 p = 0
sterge_cifra_x x c = 
    if (mod x 10) == c
        then sterge_cifra_x (div x 10) c
    else
        (sterge_cifra_x (div x 10) c) * 10 + mod x 10 


ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m n = 
    if m > 0 && n == 0
        then ackermann (m - 1) 1
    else if m > 0 && n > 0
        then ackermann (m - 1) (ackermann m (n - 1))
    else    
        -1