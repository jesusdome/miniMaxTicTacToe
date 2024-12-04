module TicTacToeProblem
(
    Estado,
    estadoInicial,
    esEstadoFinal,
    hijos
) where

import Data.Matrix (Matrix, zero, getRow, getCol, getElem, fromList, toList, getDiag)

type Estado = Matrix Int

estadoInicial :: Estado
estadoInicial = zero 3 3

esEstadoFinal :: Estado -> (Bool, Int)
esEstadoFinal s
    | fst c = c
    | fst d = d
    | esCompleto s = (True, 0)
    | otherwise = (False, 0)
    where 
        c = comprobarFilasColumnas s 1
        d = comprobarDiagonales s

esCompleto :: Estado -> Bool
esCompleto s = all (/=0) (toList s) 

comprobarFilasColumnas :: Estado -> Int -> (Bool, Int)
comprobarFilasColumnas s i
    | i > 3 = (False, 0)
    | r == 3 || c == 3 = (True, 1)
    | r == -3 || c == -3 = (True, -1)
    | otherwise = comprobarFilasColumnas s (i+1)
    where
        r = sum (getRow i s)
        c = sum (getCol i s)

comprobarDiagonales :: Estado -> (Bool, Int)
comprobarDiagonales s
    | sum (getDiag s) == 3 = (True, 1)
    | sum (getDiag s) == -3 = (True, -1)
    | ((getElem 3 1 s) + (getElem 2 2 s) + (getElem 1 3 s)) == 3 = (True, 1)
    | ((getElem 3 1 s) + (getElem 2 2 s) + (getElem 1 3 s)) == -3 = (True, -1)
    | otherwise = (False, 0)

hijos :: Estado -> [Estado]
hijos s = concat (map (\x -> hijosAux x s) [0..8])
        
hijosAux :: Int -> Estado -> [Estado]
hijosAux i s 
    |e  == 0 = [fromList 3 3 (replaceAt i 1 xs), fromList 3 3 (replaceAt i (-1) xs)]
    |otherwise = []
    where
        e = (toList s) !! i
        xs = toList s

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs