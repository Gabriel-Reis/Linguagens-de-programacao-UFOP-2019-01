-- Gabriel Augusto Requena dos Reis - 16.2.8105
data Bit = On | Off deriving (Show,Eq) 
data Formula = B Bool| E Formula Formula | Ou Formula Formula | Nao Formula deriving (Show,Eq) 

--1. Defina a funcao repetir :: a -> [a] que dado um elemento, retorna uma lista infinita desse elemento.
repetir :: a -> [a]
repetir a = [a] ++ repetir a

--2. Defina um tipo de dado chamado Bit que deve ser capaz de representa um bit.
--Em seguida crie duas funcoes :-}
-- --(a) int2bit :: Int -> [Bit], que recebe um inteiro e transforma em uma lista de bits
int2bit :: Int -> [Bit]
int2bit 0 = [Off]
int2bit 1 = [On]
int2bit x
    |mod x 2 == 0 = int2bit ( div x 2 ) ++ [Off]
    |mod x 2 == 1 = int2bit ( div x 2 ) ++ [On]

-- --(b) bit2int :: [Bit] -> Int, que recebe uma lista de bits e transforma em numero inteiro.-}
bit2int :: [Bit] -> Int
bit2int x
    | null x = 0
    | head x == On = 1*2^((length x)-1) + bit2int(tail x)
    | head x == Off = bit2int (tail x)
 
--3. Crie um tipo de dados que represente uma formula, sendo uma formula sendo:
-- Verdadeiro, Falso, E-duas formulas, Ou-duas formulas,Nao-uma formula
-- Dado que foi criado o tipo de dado Formula, defina a funcao 
--eval :: Formula -> Bool que recebe uma formula e a avalia.
eval :: Formula -> Bool
eval (B f) = f
eval (Nao f) = not(eval f)
eval (E f1 f2) = (eval f1) && (eval f2)
eval (Ou f1 f2) = (eval f1) || (eval f2)

--4. Utilizando somente compressao de lista defina as seguintes funcoes:
-- --(a) allEven :: [Int] -> [Int] que recebe uma lista e 
-- -- retorna um lista somente com numeros pares.
allEven :: [Int] -> [Int]
allEven x = [y | y <- x, even y]

-- --(b) lengths :: [[a]] -> [(Int, [a])] recebe uma lista de listas e retorna 
-- --com o tamanho desse lista e a lista correspondente.
lengths :: [[a]] -> [(Int, [a])]
lengths a = [( (length a),a ) | a <- a]

-- --(c) combinacao :: [a] -> [b] -> [(a, b)] que recebe duas lista e faz 
-- --todas as combinacoes possıveis dasduas listas
combinacao :: [a] -> [b] -> [(a, b)]
combinacao x y = [(x, y) |y <- y,x <- x]

-- --(d) divisor :: Int -> [Int] que dado um numero, 
-- --retorne a lista com todos os divisores desse numero-}
divisor :: Int -> [Int]
divisor num = [ x | x <- [1..num], num `mod` x == 0]

--5. Dado que uma formula e uma lista de lista, em que a lista mais interna 
--e uma combinacao de es e a lista mais externa e uma combinacao de ous, deﬁna a funcao
--avalia :: [[Bool]] -> Bool que transforma a lista de lista em um unico valor booleano. 
--Exemplo: (V e F e V) ou V ou (F e F) ≡ [[True,False,True],[True],[False,False]] -}
--Ex convertido:(Ou (E (B True) (E (B False) (B True))) (Ou (B True) (E (B False) (B False))))
avalia :: [[Bool]] -> Bool
avalia [] = False
avalia (x:xs) = eInterno(x) || avalia(xs)

eInterno :: [Bool] -> Bool
eInterno [] = True
eInterno x = (head x && eInterno(tail x))
 
-- 6. Dado as questoes 3 e 5 deﬁna as funcoes:
-- --(a) for2lst :: Formula -> [[Bool]] que transforma uma formula em uma lista de lista de booleano.
for2lst :: Formula -> [[Bool]]
for2lst (B a) = [[a]]
for2lst (Nao (B a)) = [[not a]]
for2lst (Nao (E a b)) = for2lst (Ou (Nao a) (Nao b))
for2lst (Nao (Ou a b)) = for2lst (E (Nao a) (Nao b))
for2lst (Ou a b) = (for2lst a) ++ (for2lst b)
for2lst (E a b) = [head(for2lst a) ++ head(for2lst b)]

-- --(b) lst2for :: [[Bool]] -> Formula que transforma uma lista de lista de booleano em uma formula.
lst2for :: [[Bool]] -> Formula
lst2for (x:[]) = eInterno' x
lst2for (x:xs) = (Ou (eInterno' x) (lst2for xs) )

eInterno' :: [Bool] -> Formula
eInterno' (x:[]) = (B x)
eInterno' (x:xs) = (E (B x) (eInterno' xs))

--7. Deﬁna a funcao iteracao :: (a -> a) -> a -> [a] que cria uma lista inﬁnita de aplicacao de fucao.
--iteracao f a = f a : f (f a) : f (f (f a)) : ...
iteracao :: (a -> a) -> a -> [a]
iteracao f a = (f a) : iteracao f (f a)

--8. Utilizando somente funcao de alta ordem (foldr, filter, (+3), etc), deﬁna as seguinte funcoes:
-- --(a) replace :: Eq a ⇒ a -> a -> [a] -> [a] que dado o primeiro e segundo parametro, 
-- --troca onde tiver o valor do primeiro parametro pelo o valor do segundo parametro.
replace :: Eq a => a -> a -> [a] -> [a]
replace troca novo lista = map (\atual -> if atual==troca then novo else atual) lista
-- replace troca novo lista = scanr1 (\atual acc -> if atual==troca then novo else atual) lista

-- --(b) int2bit0 :: Int -> [Bit] 
int2bit0 :: Int -> [Bit]
int2bit0 0 = [Off]
int2bit0 num = map (\x -> if (x `mod` 2) == 1 then On else Off) (geraLista num)

geraLista :: Int -> [Int]
geraLista 0 = []
geraLista x = geraLista (x `div` 2) ++ [x]

-- --(c) allEven0 :: [Int]− > [Int] 
allEven0 :: [Int] -> [Int]
allEven0 lista = filter (even) lista

-- --(d) avalia0 :: [[Bool]] -> Bool 
avalia0 :: [[Bool]] -> Bool
avalia0 (x:[]) = (foldr1 (&&) x)
avalia0 (x:xs) = (foldr1 (&&) x) || avalia0 xs

-- --(e) soma :: [Int] -> Int, que dado uma lista de inteiros, retorna a soma de todos os elementos-}
soma :: [Int] -> Int
soma x = foldl (+) 0 x