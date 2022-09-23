 
idade1 :: Int -- valor inteiro
idade1 = 17

testeIdade :: Bool
testeIdade = idade1>=18

--x :: Int
--x = 3

quadrado :: Int -> Int --funcao que eleva ao quadrado
quadrado x = x * x

--a :: Int
--a = 1
--b = 2

mini :: Int -> Int -> Int
mini a b
  | a <= b = a
  | otherwise = b
  
-- resolução encontrada
maioridade :: Int -> Bool
maioridade idadeInput
  | idadeInput > idade1 = True
  | otherwise = False
 
-- resolução preferivel
maiorDeIdade :: Int -> Bool
maiorDeIdade idade2 = idade2 >= 18

-- resolução preferivel
tresIguais :: Int -> Int -> Int -> Bool
tresIguais n1 n2 n3 = (n1 == n2) && (n2 == n3)

