module Exercises where

-- Exercício 1
addTwoValues a b = a + b

-- Exercício 2
isDivisible a b = 
    -- Vi que existe uma sintaxe do if com | porém achei essa mais elegante
    if a `mod` b == 0 
        then True
        else False

-- Exercício 3
divisionRemainder a b = (q, r)
    where 
        q = a `div` b
        r = a `mod` b
    -- div e / são aparentemente iguais mas / não funcionou, sei lá por quê

-- Exercício 4
torricelli v0 a deltaS =
    let v = v0**2 + (2*a*deltaS)
        answer = sqrt (v)
    in answer

-- Exercício 5
momentum m v0 a deltaS = p
    -- aceleração constante -> MRUV
    -- S = S0 + v0*t + ((a*(t**2))/2)
    -- t é uma equação do segundo grau, pegar a resposta positiva
    -- V = v0 + a*t
    -- p = m * V
    where
        isPositive n = if n > 0 then True else if n == 0 then True else False
        bhskp a b c = x
            where 
                delta = b * b - (4 * a * c)
                form op = (op (-b) (sqrt delta)) / (2 * a)
                x1 = form (+)
                x2 = form (-)
                x = if isPositive x1
                    then x1
                    else if isPositive x2
                        then x2
                        else 0
        t = bhskp (a/2) v0 (-deltaS)
        v = v0 + a*t
        p = m * v

-- Exercício 6
sumVector (a1,a2) (b1,b2) = (c1,c2)
    where
        c1 = a1 + b1
        c2 = a2 + b2

-- Exercício 7
vectorOperator (x0,y0) f (x1,y1) = f (x0,y0) (x1,y1) 

-- Exercício 8
first5Average l = result
    where
        nl = take 5 l
        result = (foldr (+) 0 nl) / 5

-- Exercício 9
headsFromList l1 l2 = (x1,y1)
    where
        x1 = head l1
        y1 = head l2

-- Exercício 10
fibonacci n = element
    where
        fib a b = a:fib b (a+b)
        fl = take n (fib 0 1)
        element = 
            if n == 0
                then 0
                else if n == 1
                    then 1
                    else fl!!(n-1) + fl!!(n-2)

-- Exercício 11
get7Multiples l = result
    where
        isMultiple7 n =
            if n `mod` 7 == 0
                then n
                else (-1)
        tl = map isMultiple7 l
        x = -1
        result = (filter (/= x) tl)

-- Exercício 12
-- vectorModule :: [(Int,Int)] -> [Int] 
vectorModule l = ml
    where
        -- moduleUnit :: (Int,Int) -> Float
        -- O retorno de sqrt é float
        moduleUnit (x0,y0) = sqrt(((x0*x0)+(y0*y0)))
        nl = map moduleUnit l
        -- Preciso converter o resultado para int
        ml = map round nl