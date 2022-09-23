
--   SEMÂNTICA FORMAL
--      TRABALHO 1 
-- EDUARDA ABREU CARVALHO

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
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | DoWhile C B  -- Do C while B
    | Repeat C B  -- repeat C until B
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
   deriving(Eq,Show)                


--------------------------------------------------------------------
--- As próximas funções, servem para manipular a memória (sigma) ---
--------------------------------------------------------------------

--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de 
-- tuplas, onde o primeiro elemento da tupla é uma String (nome da variável) e o
-- segundo um Inteiro (conteúdo da variável):

type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp", 0), ("y", 0)]

exSigma2 :: Memoria
exSigma2 = [("x", 3), ("y", 0), ("z", 0)]

exSigma3 :: Memoria
exSigma3 = [("x", 1), ("y", 20), ("z", 0)]

exSigma4 :: Memoria
exSigma4 = [("x", 4), ("y", 4), ("z", 0)]

exSigmaZero :: Memoria
exSigmaZero = [("x", 0), ("y", 0), ("z", 0)]



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
ebigStep :: (E, Memoria) -> Int

ebigStep (Var x, s) = procuraVar s x

ebigStep (Num n, s) = n

ebigStep (Soma e1 e2, s)  = ebigStep (e1, s) + ebigStep (e2, s)

ebigStep (Sub e1 e2, s) = ebigStep (e1, s) - ebigStep (e2, s)

ebigStep (Mult e1 e2, s) = ebigStep (e1, s) * ebigStep (e2, s)


---------------------------
-------- BOOLEANOS --------
---------------------------
bbigStep :: (B, Memoria) -> Bool

bbigStep (TRUE, s)  = True

bbigStep (FALSE, s) = False

bbigStep (Not b, s) 
   | bbigStep (b, s) == True = False
   | otherwise = True 

bbigStep (And b1 b2, s) 
  | bbigStep (b1, s) == False = False
  | otherwise = bbigStep (b2, s)
  
bbigStep (Or b1 b2, s)
  | bbigStep (b1, s) == True = True
  | otherwise = bbigStep (b2, s)

bbigStep (Leq e1 e2, s) = ebigStep (e1, s) <= ebigStep (e2, s)
  
bbigStep (Igual e1 e2, s) = ebigStep (e1, s) == ebigStep (e2, s)


--------------------------
-------- COMANDOS --------
--------------------------
cbigStep :: (C, Memoria) -> (C, Memoria)

cbigStep (Skip, s) = (Skip, s)


cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e, s)))

cbigStep (Seq c1 c2, s) = 
  let (c, sf) = cbigStep (c1, s)
  in cbigStep (c2, sf)

cbigStep (If b c1 c2, s)
  | bbigStep (b, s) == True = cbigStep (Seq c1 Skip, s)
  | otherwise = cbigStep (Seq c2 Skip, s)

cbigStep (While b c, s)
  | bbigStep (b, s) == True = (Seq c (While b c), s)
  | otherwise = (Skip, s)

cbigStep (DoWhile c b, s) = (Seq c (While b c), s)

cbigStep (Repeat c b, s)
  | bbigStep (b, s) == False = (Seq c (Repeat c b), s)
  | otherwise = (Skip, s)
  

----------------------------------------------------
--------- EXEMPLOS DE PROGRAMAS PARA TESTE ---------
----------------------------------------------------

--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. 
--- para rodar: "ebigStep (progExp1, exSigma)" no console

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))


-- Exemplos de Programas Booleanos:
teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

teste3 :: B
teste3 = (Leq (Soma (Num 4) (Num 3))  (Mult (Num 2) (Num 3)))


-- Exemplos de Programas Imperativos:
testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


programa1 :: C
programa1 = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                    (Leq (Var "x") (Num 5))

programa2 :: C
programa2 = If (Not (Igual (Var "x")(Var "y"))) 
               testec1
               (Seq (Atrib (Var "y") (Mult (Var "y") (Num 2)))
                    (Atrib (Var "z") (Var "y")))

