-- double recebe um inteiro e retorna o seu valor duas vezes maior
double :: Int -> Int
double t = t+t
-- double = \x -> x*2

--double2 recebe um inteiro e retorna o seu valor 4 vezes maior
double2 :: Int -> Int
double2 t = t*4
-- double = \x -> x*4

-- sel atua da seguinte forma:
-- -- Caso o primeiro parametro é True  retorna o segundo  parametro
-- -- Caso o primeiro parametro é False retorna o terceiro parametro
sel :: Bool -> Int -> Int -> Int
sel bol x y
    | bol = x
    | otherwise = y

-- max2 retorn o maior valor entre dos dois valores
-- Utilize sel para resolver esse problema
max2 :: Int -> Int -> Int
max2 x y
    |x > y = sel True x y
    |otherwise = sel False x y
-- max2 x y = sel (x>y) x y

-- max2' retorn o maior valor entre dos dois valores
-- Não sel para resolver esse problema
max2' :: Int -> Int -> Int
max2' x y
    |x > y = x
    |otherwise = y

-- max3 retorn o maior valor entre dos três valores
-- Utilize max2 ou max2' para resolver esse problema
max3 :: Int -> Int -> Int -> Int
max3 x y z
    |z > (max2' x y) = z
    |otherwise = (max2' x y)
    -- max3 x y z = x `max2` y `max2` z

-- m recebe um inteiro e, caso seja maior que 100, retorna n-10, 
-- caso contrário então retorna m (m (n+11))
m :: Int -> Int
m x
    |x > 100 = x-10
    |otherwise = m(m(x+11))
-- Qual valor esta função retorna para valores de entrada positivos 
-- menores que 101? 91

-- num2digits que recebe um inteiro e transforma em uma 
-- lista de dígitos correspondente ao número
-- -- num2digits 4721 => [4,7,2,1]
num2digits :: Int -> [Int]
num2digits x 
    | num == 0 = [x]
    | otherwise = num2digits num ++ [res]
    where
        num = x `div` 10
        res = x `mod` 10

-- Um número é dito chic se o digito resultante da soma de seus dígitos
-- ocorre no número. Se o resultado da soma dos dígitos for um número
-- com mais de um dígito, então o processo deve ser repetido até
-- que se obtenha um único dígito.
-- -- 1276 é chic, pois 1+2+7+6 = 16, 1+6 = 7.
-- -- 123 não é chic, 1+2+3 = 6
chic :: Int -> Bool
chic x = elem s num
    where
        num = num2digits x
        s = sumLoop x
        sumLoop :: Int -> Int
        sumLoop x
            | x `div` 10 == 0 = x
            | otherwise = sumLoop $ sum $ num2digits x


-----------------------------------------------------------------------------------------
-- Tipos de dados - Intro
-----------------------------------------------------------------------------------------

-- É possível implementar as frações como duplas como, por exemplo,
-- 3/5 = (3,5)
-- 2/8 = (2,8)
-- 3/4 = (3,4)

-- -- multf que recebe duas frações e faz a sua multiplicação
multf :: (Int, Int) -> (Int, Int) -> (Int, Int)
multf (a,b) (c,d) = (a*c , b*d)

-- -- somaf que recebe duas frações e faz a sua soma
somaf :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaf (a,b) (c,d) = ( (((b*d) `div` b)*a) + (((b*d) `div` d)*c), (b*d))

-- EXTRA SEM ESTAR NA LISTA
somafmmc :: (Int, Int) -> (Int, Int) -> (Int, Int)
somafmmc (a,b) (c,d) = ( ( (((b `mmc` d) `div` b )* a ) + ( ((b `mmc` d )`div` d) * c ) ) , ( b `mmc` d ) )

-- EXTRA SEM ESTAR NA LISTA
mmc ::  Int -> Int -> Int
mmc x y = (x*y) `div` (x `mdc` y)

-- EXTRA SEM ESTAR NA LISTA
mdc :: Int -> Int -> Int
mdc m n
    | m == 0 = n
    | m > 0 = mdc (n `mod` m) m

-- subf que recebe duas frações e faz a sua subtração
subf :: (Int, Int) -> (Int, Int) -> (Int, Int)
subf (a,b) (c,d) = ( (((b*d) `div` b)*a) - (((b*d) `div` d)*c), (b*d))

-- -- divf que recebe duas frações e faz a sua divisão
divf :: (Int, Int) -> (Int, Int) -> (Int, Int)
divf (a,b) (c,d) = multf (a,b) (d,c)

-- EXTRA SEM ESTAR NA LISTA
-- MENU DE FRAÇÕES
frac :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
frac x (a,b) (c,d)
    |x == 1 = somaf (a,b) (c,d)
    |x == 2 = subf (a,b) (c,d)
    |x == 3 = divf (a,b) (c,d)
    |x == 4 = multf (a,b) (c,d)

-- -- toReal que tranforma a fração em um valor real
toReal :: (Int, Int) -> Float
toReal (x,y) = (fromIntegral x) / (fromIntegral y)

-- -- LISTA 2
andTern :: Int -> Int -> Int
andTern a b 
    | a == b = b
    | a == 2 && b == 1 = 2
    | b == 2 && a == 1 = 2
    | otherwise = 0

orTern :: Int -> Int -> Int
orTern a b
    | a == 1 = a
    | b == 1 = b
    | otherwise = 0

notTern :: Int -> Int
notTern a
    | a == 1 = 2
    | a == 2 = 1
    |otherwise = 2

xorTern :: Int -> Int -> Int
xorTern a b
    | a == 2 = a
    | b == 2 = b
    | andTern a b == 1 = 0
    | otherwise = 1

-- EXTRA SEM ESTAR NA LISTA
-- MENU DE LOGICA TERNÁRIA
-- logicaTern :: Int -> Int -> Int
-- logicaTern x a b
--     |x == 1 = andTern (a,b)
--     |x == 2 = orTern (a,b)
--     |x == 3 = notTern (a,b)
--     |x == 4 = xorTern (a,b)

euclides :: Int -> Int -> Int
euclides a b
    |b == 0 = a
    |otherwise = euclides b (a `mod` b)

lcm' :: Int -> Int -> Int
lcm' a b = a*b `div` euclides a b

cosA :: Double -> Double -> Double
cosA a pres = 

-- sum = somatorio de 0 a n   cos(1)
-- Eroo == (Sum + Cos (n+1)) - sum
-- 
-- 
