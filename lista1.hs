-- exercicios
-- lista 1

-- exerc 1 :: palindromo
palindromo :: String -> Bool
palindromo texto = texto == (reverse texto)

-- exerc 2 :: (triangulo) lado 1 + lado 2 > lado3
verificaTriang :: Int -> Int -> Int -> Bool
verificaTriang l1 l2 l3 = (l1 + l2 > l3) && (l1 + l3 > l2) && (l2 + l3 > l1)

-- exerc 3 :: -1 se valor<0; 1 se valor>0; 0 se valor == 0
sinal :: Int -> Int
sinal num 
  | num < 0 = -1
  | num > 0 = 1
  | otherwise = 0

-- exerc 4 :: menor de tres
menorTres :: Int -> Int -> Int -> Int
menorTres n1 n2 n3
  | (n1 <= n2) && (n1 <= n3) = n1
  | (n2 <= n3) = n2
  | otherwise = n3


-- exerc 5 :: potenciação base ^ expoente
pot :: Int -> Int -> Int
pot num 0 = 1
pot num 1 = num
pot num exp = num * pot num (exp-1)

