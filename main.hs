import Data.Matrix
import Data.List

type Estado = Matrix Int

estadoInicial :: Estado
estadoInicial = zero 3 3

esEstadoFinal :: Estado -> (Bool, Int)
esEstadoFinal s
    | fst c = c
    | fst d = d
    | otherwise = (False, 0)
    where 
        c = comprobarFilasColumnas s 1
        d = comprobarDiagonales s

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

data ArbolDeEstados a = N1 a [ArbolDeEstados a]
                    deriving (Eq, Show)

data MiniMaxTree a = N2 Int a [MiniMaxTree a]
                    deriving (Eq, Show)

generarArbolDeEstados ::  Estado -> ArbolDeEstados Estado
generarArbolDeEstados s
    | b = (N1 s [])
    | otherwise = (N1 s (map generarArbolDeEstados (hijos s)))
    where
        (b, p) = esEstadoFinal s

generaArbolMiniMax :: ArbolDeEstados Estado -> Bool -> MiniMaxTree Estado
generaArbolMiniMax (N1 s []) _ = N2 (snd (esEstadoFinal s)) s []
generaArbolMiniMax (N1 s xs) b =
    if b
        then
            N2 (maximum (map (\x -> let N2 v _ _ = generaArbolMiniMax x False in v) xs)) s (map (\x -> generaArbolMiniMax x False) xs)
        else
            N2 (minimum (map (\x -> let N2 v _ _ = generaArbolMiniMax x True in v) xs)) s (map (\x -> generaArbolMiniMax x True) xs)

mejorJugada :: MiniMaxTree Estado -> Estado -> Bool -> Estado
mejorJugada (N2 _ s xs) es b
    | s == es =
        if b
            then estadoDelNodoActual (maximumBy (\(N2 v1 _ _) (N2 v2 _ _) -> compare v1 v2) xs)
            else estadoDelNodoActual (minimumBy (\(N2 v1 _ _) (N2 v2 _ _) -> compare v1 v2) xs)
    | otherwise = 
        case filter (containsEstado es) xs of
            []     -> error "Estado no encontrado en el árbol"
            (x:_) -> mejorJugada x es b

estadoDelNodoActual :: MiniMaxTree Estado -> Estado
estadoDelNodoActual (N2 _ s _) = s

containsEstado :: Estado -> MiniMaxTree Estado -> Bool
containsEstado es (N2 _ s xs)
    | s == es  = True
    | otherwise = any (containsEstado es) xs

