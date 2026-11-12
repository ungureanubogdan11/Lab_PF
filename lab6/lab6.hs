

data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = elem s ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia (Mar _ _) = False

test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum [y | (Portocala x y) <- l, ePortocalaDeSicilia (Portocala x y)]
    
test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = sum [if y then 1 else 0 | (Mar x y) <- l]

test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ rasa) = Just rasa
rasa (Pisica _) = Nothing

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M l) x = foldr ((&&) . (== x) . (sumvals)) True l
sumvals (L vals) = sum vals  
test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

doarPozN :: Matrice -> Int -> Bool
doarPozN (M ls) x = foldr (\(L l) acC -> if (length l) /= x then True else (foldr (\a acc -> (a >= 0) && acc) True l) && acC) True ls

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False
corect :: Matrice -> Bool
corect (M m) = foldr (\(L l) acc -> (length l == length (scoate (m!!0))) && acc) True m 

scoate (L l) = l

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True

type Position = (Int, Int)
data Orientation = N | NE | E | SE | S | SW | W | NW
    deriving (Eq, Enum)

type Turtle = (Position, Orientation)

data Action = Step | Turn | Seq Command Command
    deriving Show

data Command = Do Action | Repeat Action Int | Wait
    deriving Show

getPizza :: Turtle -> [Command] -> Position
getPizza (pos, _) [] = pos

getPizza (pos, or) ((Repeat x 0) : rest) = getPizza(pos, or) rest
getPizza (pos, or) ((Repeat x k) : rest) = getPizza(pos, or) (Do x : (Repeat x (k - 1)) : rest) 

getPizza ((x, y), orientare) ((Do Step) : rest) = 
    if orientare == N then getPizza ((x, y + 1), orientare) rest
    else if orientare == NE then getPizza ((x + 1, y + 1), orientare) rest
    else if orientare == E  then getPizza ((x + 1, y), orientare) rest
    else if orientare == SE then getPizza ((x + 1, y - 1), orientare) rest
    else if orientare == S  then getPizza ((x, y - 1), orientare) rest
    else if orientare == SW then getPizza ((x - 1, y - 1), orientare) rest
    else if orientare == W  then getPizza ((x - 1, y), orientare) rest
    else getPizza ((x + 1, y - 1), orientare) rest  -- NW

getPizza  (pos, orientare) (Do Turn : rest) =
    getPizza (pos, toEnum ((fromEnum orientare + 1) `mod` 8)) rest

getPizza turtle (Wait : rest) = getPizza turtle rest

getPizza turtle (Do (Seq x y) : rest) = getPizza turtle (x : y : rest)

agregare :: [Command] -> Command
agregare = foldr (\x acc -> Do (Seq x acc)) Wait

