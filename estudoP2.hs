fatorial :: Int -> Int
fatorial x
    |x == 1 = 1
    |otherwise = x * fatorial (x-1)

fibb :: Int -> Int
fibb x 
    |x == 0 = 0
    |x == 1 = 1
    |otherwise = fibb(x-1) + fibb(x-2)

sumTupla :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumTupla (a,b) (c,d) = ((a+c),(b+d))

sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (x:xs) = x+ sumAll xs

sizeList :: [a] -> Int
sizeList [] = 0
sizeList (x:xs) = 1 + sizeList xs

sizeList' :: [a] -> Int
sizeList' a
    |null a = 0
    |otherwise = 1 + sizeList' (tail a)

compLista :: (Eq a) => [a] -> [a] -> Bool
compLista a b
    |null a && null b = True
    |null a || null b = False
    |head a == head b = compLista (tail a) (tail b)

invLista :: [a] -> [a]
invLista [] = []
invLista (x:xs) = invLista xs ++ [x]

pertence :: (Eq a) => [a] -> a -> Bool
pertence [] _ = False
pertence (x:xs) elem | x == elem = True
pertence (_:xs) elem = pertence xs elem

maior :: [Int] -> Int
maior [x] = x
maior (x:xs)
    | x > (maior xs) = x
    | otherwise = maior xs

todospares :: [Int] -> Bool
todospares [] = False
todospares [x] = even x
todospares (x:xs)
    | (x `mod` 2 /= 0) = False
    | otherwise = todospares xs

concatena :: [a] -> [a] -> [a]
concatena a b = a++b

concatena' :: [a] -> [a] -> [a]
concatena' [] b = b
concatena' a [] = a
concatena' (a:axs) b = a:concatena' axs b

ehPrimo :: Int -> Bool
ehPrimo n 
    | n == 1 = False
    | length [x | x <- [2 .. n-1], n `mod` x == 0] > 0 = False
    | otherwise = True

mesmaSoma :: [[Int]] -> Bool
mesmaSoma [] = True
mesmaSoma [x] = True
mesmaSoma (x:xs)
    | sum x == sum (head xs) = True && mesmaSoma xs
    | otherwise = False

fill :: String -> Int -> String
fill [] _ = ""
fill texto n =  take n (texto) ++ "   " ++ fill (drop n texto) n

insereNaLista :: [Int] -> Int -> Int -> [Int]
insereNaLista lista elem pos = take pos lista ++ [elem] ++ drop pos lista

removePosNaLista :: [Int] -> Int -> [Int]
removePosNaLista lista pos = take pos lista ++ drop (pos+1) lista

ordenaDec :: [Int] -> [Int]
ordenaDec [] = []
ordenaDec (x:xs)
        |null xs = [x]
        |x > maximum xs = x:ordenaDec xs
        |otherwise = maximum xs:ordenaDec nx
        where nx = removeElemNaLista (x:xs) (maximum xs)

ordenaCre :: [Int] -> [Int]
ordenaCre x = reverse (ordenaDec x)

ordenaCre' :: [Int] -> [Int]
ordenaCre' [] = []
ordenaCre' (x:xs)
        |null xs = [x]
        |x < minimum xs = x:ordenaCre' xs
        |otherwise = minimum xs:ordenaCre' nx
        where nx = removeElemNaLista (x:xs) (minimum xs)


removeElemNaLista :: [Int] -> Int -> [Int]
removeElemNaLista [] _ = []
removeElemNaLista (x:xs) elem
        | null xs && x == elem = []
        | null xs = [x]
        | x == elem = xs
        | otherwise = [x] ++ (removeElemNaLista xs elem)


-- Defina uma função que, dada uma lista numérica, retorna uma tupla-2,
-- tal que contenha o maior valor da lista, bem como sua posição relativa

maiorPos :: [Int] -> (Int, Int)
maiorPos x = (maximum x, posicaoDeX x (maximum x))

posicaoDeX :: [Int] -> Int -> Int
posicaoDeX [] _ = -1
posicaoDeX (x:xs) elem
    | elem == x = 1
    | otherwise = 1 + posicaoDeX xs elem