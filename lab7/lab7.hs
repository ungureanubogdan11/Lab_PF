import Data.List (nub, (\\))
import Data.Maybe (fromJust)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q")))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: (((Not (Var "P")) :|: (Not (Var "Q"))) :&: ((Not (Var "P")) :|: (Not (Var "R"))))

instance Show Prop where
  show (Var nume) = nume
  show T = "T"
  show F = "F"
  show (Not p) = "(~" ++ show p ++ ")"
  show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
  show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
  show (p :->: q)  = "(" ++ show p ++ "->"  ++ show q ++ ")"
  show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var nume) env = impureLookup nume env 
eval T env = True
eval F env = False
eval (Not nume) env = not (eval nume env)
eval (p :|: q) env = (eval p env) || (eval q env)
eval (p :&: q) env = (eval p env) && (eval q env)
eval (p :->: q)  env = not (eval p env) || eval q env
eval (p :<->: q) env = eval p env == eval q env
 
test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

variabile :: Prop -> [Nume]
variabile p = map (:[]) (nub (show p) \\ "~()|&<>-:!@@#$%^~'")
 
test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]


configs :: Int -> [[Bool]]
configs 0 = [[]]
configs n = [b : rest | b <- [False, True], rest <- configs (n - 1)]

envs :: [Nume] -> [Env]
envs x = map (zip x) (configs (length x))
 
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

satisfiabila :: Prop -> Bool
satisfiabila x = foldr (\v acc -> (eval x v) || acc) False (envs (variabile x))
 
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida x = satisfiabila (Not x) == False

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True


echivalenta :: Prop -> Prop -> Bool
echivalenta x y = foldr (\v acc -> ((eval x v) == (eval y v)) && acc) True (envs (nub ((variabile x) ++ (variabile y)))) 
 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))

