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
