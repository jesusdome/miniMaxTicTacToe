module MiniMaxTAD
(
    ArbolDeEstados,
    MiniMaxTree,
    generaArbolMiniMax,
    generarArbolDeEstados,
    mejorJugada,
    emptyTree
) where

import Data.List (maximumBy, minimumBy)

data ArbolDeEstados a = N1 a [ArbolDeEstados a]
                    deriving (Eq, Show)

data MiniMaxTree a = N2 Int a [MiniMaxTree a]
                    deriving (Eq, Show)

emptyTree :: a -> MiniMaxTree a
emptyTree s = (N2 0 s [])
{-
generarArbolDeEstados :: (Eq a) => a -> (a->[a]) -> (a -> (Bool, Int)) -> ArbolDeEstados a
generarArbolDeEstados s hijos esEstadoFinal
    | b = (N1 s [])
    | otherwise = (N1 s (map (\x -> generarArbolDeEstados x hijos esEstadoFinal) (hijos s) ))
    where
        (b, p) = esEstadoFinal s


generaArbolMiniMax :: (Eq a) => ArbolDeEstados a -> Bool -> (a -> (Bool, Int)) -> MiniMaxTree a
generaArbolMiniMax (N1 s []) _ esEstadoFinal = N2 (snd (esEstadoFinal s)) s []
generaArbolMiniMax (N1 s xs) b esEstadoFinal =
    if b
        then
            N2 (maximum (map (\x -> let N2 v _ _ = generaArbolMiniMax x False esEstadoFinal in v) xs)) s (map (\x -> generaArbolMiniMax x False esEstadoFinal) xs)
        else
            N2 (minimum (map (\x -> let N2 v _ _ = generaArbolMiniMax x True esEstadoFinal in v) xs)) s (map (\x -> generaArbolMiniMax x True esEstadoFinal) xs)
-}

generarArbolDeEstados :: (Eq a) => a -> (a -> [a]) -> (a -> Int -> (Bool, Int)) -> Int -> ArbolDeEstados a
generarArbolDeEstados s hijos esEstadoFinal n
    | b = (N1 s [])
    | otherwise = (N1 s (map (\x -> generarArbolDeEstados x hijos esEstadoFinal n) (hijos s)))
    where
        (b, p) = esEstadoFinal s n

generaArbolMiniMax :: (Eq a) => ArbolDeEstados a -> Bool -> (a -> Int -> (Bool, Int)) -> Int -> MiniMaxTree a
generaArbolMiniMax (N1 s []) _ esEstadoFinal n = N2 (snd (esEstadoFinal s n)) s []
generaArbolMiniMax (N1 s xs) b esEstadoFinal n =
    if b
        then
            N2 (maximum (map (\x -> let N2 v _ _ = generaArbolMiniMax x False esEstadoFinal n in v) xs)) 
               s 
               (map (\x -> generaArbolMiniMax x False esEstadoFinal n) xs)
        else
            N2 (minimum (map (\x -> let N2 v _ _ = generaArbolMiniMax x True esEstadoFinal n in v) xs)) 
               s 
               (map (\x -> generaArbolMiniMax x True esEstadoFinal n) xs)



mejorJugada :: (Eq a) => MiniMaxTree a -> a -> Bool -> a
mejorJugada (N2 _ s xs) es b
    | s == es =
        if b
            then estadoDelNodoActual (maximumBy (\(N2 v1 _ _) (N2 v2 _ _) -> compare v1 v2) xs)
            else estadoDelNodoActual (minimumBy (\(N2 v1 _ _) (N2 v2 _ _) -> compare v1 v2) xs)
    | otherwise = 
        case filter (containsEstado es) xs of
            []     -> error "Estado no encontrado en el árbol"
            (x:_) -> mejorJugada x es b

estadoDelNodoActual :: (Eq a) => MiniMaxTree a -> a
estadoDelNodoActual (N2 _ s _) = s

containsEstado :: (Eq a) => a -> MiniMaxTree a -> Bool
containsEstado es (N2 _ s xs)
    | s == es  = True
    | otherwise = any (containsEstado es) xs
