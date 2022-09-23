-- exercicios
-- lista 2

-- exerc 1 :: n * lista
multLista :: Int -> [Int] -> [Int]
multLista n [] = []
multLista n (x:xs) = n*x : multLista n xs

-- exerc 2 :: se N está em Lista
elemento :: Int -> [Int] -> Bool
elemento n [] = False
elemento n (x:xs) = (n == x) || (elemento n xs)

-- exerc 3 :: quantas vezes N está em Lista
conta :: Int -> [Int] -> Int
conta n [] = 0
conta n (x:xs) 
  | n == x = 1 + conta n xs
  | otherwise = 0 + conta n xs

-- exerc 4 :: quantos num em Lista são > N
contaMaiores :: Int -> [Int] -> Int
contaMaiores n [] = 0
contaMaiores n (x:xs)
  | x > n = 1 + contaMaiores n xs
  | otherwise = 0 + contaMaiores n xs
  
 -- exerc 5 :: retorna numeros de Lista > N
maiores :: Int -> [Int] -> [Int]
maiores n [] = []
maiores n (x:xs)
  | x > n = x : maiores n xs   
  | otherwise = maiores n xs 

-- exerc 6 :: retorna lista contendo o Y repetido X vezes dado (X Y)
geraLista :: Int -> Int -> [Int]
geraLista x y 
  | (x < 0) || (x == 0) = []
  | otherwise = y : geraLista (x-1) y
  
-- exerc 7 :: adiciona N no fim da Lista
addFim :: Int -> [Int] -> [Int]
addFim n [] = [n]
addFim n (x:xs) = x : addFim n xs
--addFim a l = l ++[a] ?????
  
-- exerc inverter lista
--inverte :: [Int] -> [Int]
--inverte [] = []
--inverte (x:xs) = xs : [x]

-- exerc tamanho da lista
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs


