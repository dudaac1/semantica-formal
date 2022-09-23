-- LISTAS

-- abc == ['a','b','c'] :: retorna True

-- True : False : False : [] == [True, False, False]

--  operação ('cons' ou ':') associativa a direita
--    adiciona False a []
--    adiciona False a [False]
--    adiciona True a [False, False]

--  as listas devem ter apenas um tipo de variavel associado a si

-- 1 : [] == [1]
-- (1,2) : (3,4) : [] == [(1,2), (3,4)]

-- funções são com recursão
-- costumam ter apenas dois casos: lista vazia e lista não vazia

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista(xs)

multDois :: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs
