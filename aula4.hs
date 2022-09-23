-- Implementar a Semântica de Expressões Booleanas em Haskell
data B = TRUE | FALSE | Not B | And B B | Or B B
   deriving (Eq,Show)


-- Expressão: TRUE && FALSE || TRUE
prog1 :: B
prog1 = Or (And TRUE FALSE) TRUE

-- Expressão: TRUE && FALSE || FALSE && TRUE 
prog2 :: B
prog2 = Or (And TRUE FALSE) (And FALSE TRUE)

-- A função bigStepB recebe como entrada uma expressão booleana na nossa linguagem,
-- e devolve um booleano do Haskell (o resultado da avaliação da expressão de entrada):

bigStepB :: B -> Bool
bigStepB TRUE = True
bigStepB FALSE = False

-- As duas regras do Not:
bigStepB (Not b)
  | bigStepB b == True = False
  | otherwise = True

-- As duas regras do And:
bigStepB (And b1 b2) 
  | (bigStepB b1 == False) = False
  | otherwise = bigStepB b2
  
-- As duas regras do Or:
bigStepB (Or b1 b2) 
   | (bigStepB b1 == True) = True
   | otherwise = bigStepB b2
   
   
------------
--let(C, s) = cbigStep C1, s1 in []

