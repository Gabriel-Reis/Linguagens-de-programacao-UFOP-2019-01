-- Gabriel Augusto Requena dos Reis - 16.2.8105
-- Exercício 1 a
allOdd :: [Int] -> [Int]
allOdd x = filter (odd) x

-- Exercício 1 b
strip :: Eq a => [a] -> [a] -> [a]
strip [] b = b
strip (a:xs) b = strip xs (filter (/=a) b)

-- Exercício 1 c
concatL :: [[a]] -> [a]
concatL a = foldl (++) [] a

-- Exercício 1 d
tamanho :: [a] -> Int
tamanho x = foldl (\xs _ -> xs+1) 0 x

-- Exercício 2 a
conta :: Eq a => a -> [a] -> Int
conta elem xs = length [x | x<-xs, x==elem]

-- Exercício 2 b
ordenado :: Ord a => [a] -> Bool
ordenado xs = and [x <= y | (x,y) <- (zip xs (tail xs)) ]

-- Exercício 2 c
tamanho' :: [a] -> Int
tamanho' lista = sum [1 | x<-lista]

-- Exercício 3 - definição base de árvore
data Tree a = Leaf 
    | Node ( Tree a ) a ( Tree a )
    deriving Show

-- Exercício 3 a
addTree :: (Ord a) => Tree a -> a -> Tree a
addTree (Leaf) adicionar = Node Leaf adicionar Leaf
addTree (Node esq a dir) adicionar
   | adicionar > a = Node esq a (addTree dir adicionar)
   | otherwise = Node (addTree
    esq adicionar) a dir

-- Exercício 3 b
alturaTree :: Tree a -> Int
alturaTree Leaf = 0
alturaTree (Node esq a dir) = 1 + max 
                (alturaTree esq) (alturaTree dir)

-- Exercício 3 c
mapT :: (a -> b) -> Tree a -> Tree b
mapT _ Leaf = Leaf
mapT f (Node esq a dir) = Node (mapT(f) esq) (f a) (mapT(f) dir)