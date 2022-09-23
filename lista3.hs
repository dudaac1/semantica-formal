data Arvore = Folha Int | Nodo Int Arvore Arvore
  deriving(Eq,Show)
  
-- exerc 1
multArvore :: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (x * n)
multArvore x (Nodo n a1 a2) = Nodo (x * n) (multArvore x a1) (multArvore x a2)

-- exerc 2
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = 0 + (contaFolhas a1) + (contaFolhas a2)

-- exerc 3
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n a1 a2) = 1 + (contaNodos a1) + (contaNodos a2)

-- exerc 4
quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha n) 
  | x == n = 1
  | otherwise = 0
quantasVezes x (Nodo n a1 a2)
  | x == n = 1 + (quantasVezes x a1) + (quantasVezes x a2)
  | otherwise = 0 + (quantasVezes x a1) + (quantasVezes x a2)
  
-- exerc 5
-- max 4 33 >> retorna 33, ou seja, o maior número
maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = max (max n (maxArvore a1))(max n (maxArvore a2))

-- exerc 6
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = (Folha n)
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

-- exerc 7
geraLista :: Arvore -> [Int]
geraLista (Folha n) = n : []
geraLista (Nodo n a1 a2) = n : (geraLista a1) ++ (geraLista a2) 
