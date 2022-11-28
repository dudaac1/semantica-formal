--    SEMÂNTICA FORMAL
-- TRABALHO 2 -- SMALL-STEP 
--  EDUARDA ABREU CARVALHO


-- Definição das árvore sintática para representação dos programas:
data E = Num Int
      | Var String
      | Soma E E
      | Sub E E
      | Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | DoWhile C B  -- Do C while B
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
   deriving(Eq,Show)                


--------------------------------------------------------------------
--- As próximas funções, servem para manipular a memória (sigma) ---
--------------------------------------------------------------------

-- A próxima linha de código diz que o tipo memória é equivalente a uma lista de 
-- tuplas, onde o primeiro elemento da tupla é uma String (nome da variável) e o
-- segundo um Inteiro (conteúdo da variável):

type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

--- A função procuraVar recebe uma memória, o nome de uma variável e retorna 
--- o conteúdo dessa variável na memória. Exemplo:
--- *Main> procuraVar exSigma "x"
--- 10
procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo 
--- conteúdo para essa variável e devolve uma nova memória modificada com 
--- a varíável contendo o novo conteúdo. A chamada
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
--- essa chamada é equivalente a operação exSigma[temp->20]
mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


----------------------------
-------- EXPRESSÕES --------
----------------------------
smallStepE :: (E, Memoria) -> (E, Memoria) 

-- regra VAR
smallStepE (Var x, s) = (Num (procuraVar s x), s)

-- regra SOMA3
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)

-- regra SOMA2
smallStepE (Soma (Num n) e, s) = 
   let (el, sl) = smallStepE (e, s)
   in (Soma (Num n) el, sl)

-- regra SOMA1
smallStepE (Soma e1 e2, s) =
   let (el, sl) = smallStepE (e1, s)
   in (Soma el e2, sl)

-- regra MULT3
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)

-- regra MULT2
smallStepE (Mult (Num n) e, s) = 
   let (el, sl) = smallStepE (e, s)
   in (Mult (Num n) el, sl)

-- regra MULT1
smallStepE (Mult e1 e2, s) =
   let (el, sl) = smallStepE (e1, s)
   in (Mult el e2, sl)

-- regra SUB3
smallStepE (Sub (Num n1) (Num n2), s) = (Num (n1 - n2), s)

-- regra SUB2
smallStepE (Sub (Num n) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Sub (Num n) expr, sigma)

-- regra SUB1
smallStepE (Sub e1 e2, s) =
   let (expr, sigma) = smallStepE (e1, s)
   in (Sub expr e2, sigma)

---------------------------
-------- BOOLEANOS --------
---------------------------
smallStepB :: (B, Memoria) -> (B, Memoria)

smallStepB (Not FALSE, s) = (TRUE, s) -- NOT3
smallStepB (Not TRUE, s) = (FALSE, s) -- NOT2
smallStepB (Not b, s) =               -- NOT1
   let (bool, sigma) = smallStepB (b, s)
   in (Not bool, sigma)

smallStepB (And FALSE b, s ) = (FALSE, s) -- AND3
smallStepB (And TRUE b, s ) = (b, s)      -- AND2
smallStepB (And b1 b2, s ) =              -- AND1
   let (bool, sigma) = smallStepB (b1, s)
   in (And bool b2, sigma)

smallStepB (Or FALSE b, s ) = (b, s)    -- OR3
smallStepB (Or TRUE b, s ) = (TRUE, s)  -- OR2
smallStepB (Or b1 b2, s ) =             -- OR1
   let (bool, sigma) = smallStepB (b1, s)
   in (Or bool b2, sigma)

-- regra LEQ3
smallStepB (Leq (Num n1) (Num n2), s) 
   | n1 <= n2 = (TRUE, s)
   | otherwise = (FALSE, s)

-- regra LEQ2
smallStepB (Leq (Num n) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Leq (Num n) expr, sigma)

-- regra LEQ1
smallStepB (Leq e1 e2, s) = 
   let (expr, sigma) = smallStepE (e1, s)
   in (Leq expr e2, sigma)

-- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
-- regra IGUAL3
smallStepB (Igual (Num n1) (Num n2), s) 
   | n1 == n2 = (TRUE, s)
   | otherwise = (FALSE, s)

-- regra IGUAL2
smallStepB (Igual (Num n) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Igual (Num n) expr, sigma)

-- regra IGUAL1
smallStepB (Igual e1 e2, s) = 
   let (expr, sigma) = smallStepE (e1, s)
   in (Igual expr e2, sigma)


--------------------------
-------- COMANDOS --------
--------------------------
smallStepC :: (C, Memoria) -> (C, Memoria)

smallStepC (If FALSE c1 c2, s) = (c2, s) -- IF3
smallStepC (If TRUE c1 c2, s) = (c1, s)  -- IF2
smallStepC (If b c1 c2, s) =             -- IF1
   let (bool, sigma) = smallStepB (b, s)
   in (If bool c1 c2, sigma)

smallStepC (Seq Skip c2, s) = (c2, s)  -- SEQ2
smallStepC (Seq c1 c2,s) =             -- SEQ1
   let (cmd, sigma) = smallStepC (c1, s)
   in (Seq cmd c2, sigma)

-- regra ATRIB2
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)

-- regra ATRIB1
smallStepC (Atrib (Var x) e, s) =
   let (expr, sigma) = smallStepE (e, s)
   in (Atrib (Var x) expr, sigma)
   
smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)

smallStepC (DoWhile c b,s) = (Seq c (While b c), s)

----------------------
--  INTERPRETADORES --
----------------------

-- Expressões Aritméticas
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False

interpretadorE :: (E, Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

-- Expressões Booleanas
isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))

-- Linguagem Imperativa
isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

interpretadorC :: (C, Memoria) -> (C, Memoria)
interpretadorC (c, s) = if (isFinalC c) then (c, s) else interpretadorC (smallStepC (c, s))


----------------------------------------------------
--------- EXEMPLOS DE PROGRAMAS PARA TESTE ---------
----------------------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

exSigma3 :: Memoria
exSigma3 = [("x",0), ("y",0), ("z",0)]

exSigma4 :: Memoria
exSigma4 = [("x",5), ("y",0), ("z",0)]

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

progExp2 :: E
progExp2 = Sub (Num 3) (Soma (Var "x") (Var "y"))

---------------------------------
--------- PARA EXECUTAR ---------
---------------------------------
-- A função smallStepE anda apenas um passo na avaliação da Expressão
-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])
-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:
-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])

-- Exemplos de Programas Booleanos:
teste1 :: B
-- 3+3 <= 2*3
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
-- x+3 <= 2*3
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


-- Exemplos de Programas Imperativos:
testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- exemplo de programa usando Do-While
-- Do (x=x+1) while (x<=5)
programa1 :: C
programa1 = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                    (Leq (Var "x") (Num 5))

-- exemplo de programa usando o If
-- If (x!=y) then (testec1) else (y=y*2;z=y)
programa2 :: C 
programa2 = If (Not (Igual (Var "x")(Var "y"))) 
               testec1 --swap
               (Seq (Atrib (Var "y") (Mult (Var "y") (Num 2)))
                    (Atrib (Var "z") (Var "y")))
