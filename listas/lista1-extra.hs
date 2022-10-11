-- exercicios
-- lista 1 - extra

-- exerc 1 :: quatro numeros iguais
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais n1 n2 n3 n4 = (n1 == n2) && (n2 == n3) && (n3 == n4)

-- exerc 2 :: de três numeros, quantos são iguais
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais n1 n2 n3
  | (n1 == n2) && (n2 == n3) = 3
  | ((n1 == n2) && (n2 /= n3)) || ((n2 == n3) && (n1 /= n3)) || ((n1 == n3) && (n1 /= n2)) = 2
  | otherwise = 0
  
-- exerc 3 :: três numeros diferentes
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes n1 n2 n3 = (n1 /= n2) && (n2 /= n3) && (n1 /= n3)

-- exerc 4 (dissertativa)
-- o que está errado com a seguinte definição de todosDiferentes:
-- todosDiferentes n m p = ((n /= m) && (m /= p))?
-- R: se o retorno for True, garante que n seja diferente de m e que m seja diferente de p
--    porém não há garantia que n seja diferente de p e, portanto, não se pode afirmar se
--    todos os três números são diferentes entre si.

-- exerc 5 :: quantosSaoIguais usando todosDiferentes e todosIguais
todosIguais :: Int -> Int -> Int -> Bool
todosIguais n1 n2 n3 = (n1 == n2) && (n2 == n3)

quantosSaoIguaisV2 :: Int -> Int -> Int -> Int
quantosSaoIguaisV2 n1 n2 n3
  | todosIguais n1 n2 n3 = 3
  | todosDiferentes n1 n2 n3 = 0
  | otherwise = 2
  
-- exerc 6 :: retorna n^2
elevadoDois :: Int -> Int
elevadoDois num = num * num

-- exerc 7 :: retorna n^4 usando elevadoDois
elevadoQuatro :: Int -> Int
elevadoQuatro num = (elevadoDois num) * (elevadoDois num)

-- exerc 8 :: soma de vendas da semana 0 até semana N
-- ???
vendas :: Int -> Int
vendas n = (n+1)

vendaTotal :: Int -> Int
vendaTotal n
  | n < 0 = 0
  | n == 0 = vendas 0
  | n > 0 = vendas n + vendas (n-1)
   
 
