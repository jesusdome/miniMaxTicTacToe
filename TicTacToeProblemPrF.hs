{-
module TicTacToeProblemPrF (
    Estado,
    estadoInicial,
    esEstadoFinal,
    hijos
) where

import Data.Matrix --(Matrix, zero, getRow, getCol, getElem, fromList, toList, getDiag)

type Estado = Matrix Int

-- Estado inicial para un tablero con filas y columnas dinámicas
estadoInicial :: Int -> Int -> Estado
estadoInicial filas columnas = zero filas columnas


esEstadoFinal :: Estado -> Int -> (Bool, Int)
esEstadoFinal s n = 
    if fst c then c
    else if fst d then d
    else if esCompleto s then (True, 0)
    else (False, 0)
  where 
    c = comprobarFilasColumnas s n 1
    d = comprobarDiagonales s n

-- Función para comprobar si el tablero está completo
esCompleto :: Estado -> Bool
esCompleto s = all (/= 0) (toList s)

-- Comprobar filas y columnas
comprobarFilasColumnas :: Estado -> Int -> Int -> (Bool, Int)
comprobarFilasColumnas s n i
    | i > nrows s = (False, 0)
    | r == n || c == n = (True, 1)
    | r == -n || c == -n = (True, -1)
    | otherwise = comprobarFilasColumnas s n (i + 1)
    where
        r = sum (getRow i s)
        c = sum (getCol i s)



-- Generar todas las diagonales posibles de tamaño n
generarDiagonales :: Estado -> Int -> Int -> [[Int]]
generarDiagonales s n size =
    let
        -- Diagonales descendentes (de arriba a la izquierda hacia abajo a la derecha)
        diagonalesDescendentes =
            [ [getElem (i + j + 1) (j + 1) s | j <- [0..n-1]]
            | i <- [0..size - n] ]  -- Asegurarse de no sobrepasar los límites de la matriz

        -- Diagonales ascendentes (de abajo a la izquierda hacia arriba a la derecha)
        diagonalesAscendentes =
            [ [getElem (i - j + 1) (j + 1) s | j <- [0..n-1]]
            | i <- [n-1..size-1] ]  -- Asegurarse de no sobrepasar los límites de la matriz

        -- Diagonales descendentes (de izquierda a derecha, empezando por las primeras columnas)
        diagonalesDescendentes2 =
            [ [getElem (i + j + 1) (j + i + 1) s | j <- [0..n-1]]
            | i <- [0..size - n] ] 

        -- Diagonales ascendentes (de derecha a izquierda, empezando por las últimas columnas)
        diagonalesAscendentes2 =
            [ [getElem (i - j + 1) (size - j) s | j <- [0..n-1]]
            | i <- [n-1..size-1] ]
        
        -- Diagonales descendentes adicionales (de arriba hacia abajo, de derecha a izquierda)
        diagonalesDescendentes3 =
            [ [getElem (i + j + 1) (size - j) s | j <- [0..n-1]]
            | i <- [0..size - n] ]

        -- Diagonales ascendentes adicionales (de abajo hacia arriba, de izquierda a derecha)
        diagonalesAscendentes3 =
            [ [getElem (i - j + 1) (n - j) s | j <- [0..n-1]]
            | i <- [n-1..size-1] ]
    in diagonalesDescendentes ++ diagonalesAscendentes ++ diagonalesDescendentes2 ++ diagonalesAscendentes2 ++ diagonalesDescendentes3 ++ diagonalesAscendentes3

-- Comprobar todas las diagonales (descendentes y ascendentes)
comprobarDiagonales :: Estado -> Int -> (Bool, Int)
comprobarDiagonales s n = 
    let size = nrows s
        allDiags = generarDiagonales s n size
    in case filter (\diag -> sum diag == n || sum diag == -n) allDiags of
        [] -> (False, 0)  -- No se encontró ninguna diagonal ganadora
        (d:_) -> (True, if sum d == n then 1 else -1)



-- Función que genera los hijos de un estado
hijos :: Estado -> [Estado]
hijos s = concatMap (\x -> hijosAux x s) [0..(nrows s * ncols s - 1)]

-- Función auxiliar para generar los hijos de un estado en la posición i
hijosAux :: Int -> Estado -> [Estado]
hijosAux i s
    | e == 0 = [fromList (nrows s) (ncols s) (replaceAt i (verTurno s) xs)]
    | otherwise = []
    where
        e = (toList s) !! i
        xs = toList s

-- Función para determinar el turno
verTurno :: Estado -> Int
verTurno s = if pos == neg then 1 else -1
    where
        pos = length (filter (==1) (toList s))
        neg = length (filter (==(-1)) (toList s))

-- Reemplazar el valor en la posición i de la lista xs
replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs
-}



module TicTacToeProblemPrF (
    Estado,
    estadoInicial,
    esEstadoFinal,
    hijos
) where

import Data.Matrix --(Matrix, zero, getRow, getCol, getElem, fromList, toList, getDiag)

type Estado = Matrix Int

-- Estado inicial para un tablero con filas y columnas dinámicas
estadoInicial :: Int -> Int -> Estado
estadoInicial filas columnas = zero filas columnas


esEstadoFinal :: Estado -> Int -> (Bool, Int)
esEstadoFinal s n = 
    if fst c then c
    else if fst d then d
    else if esCompleto s then (True, 0)
    else (False, 0)
  where 
    c = comprobarFilasColumnas s n 1
    d = comprobarDiagonales s n

-- Función para comprobar si el tablero está completo
esCompleto :: Estado -> Bool
esCompleto s = all (/= 0) (toList s)

-- Comprobar filas y columnas
comprobarFilasColumnas :: Estado -> Int -> Int -> (Bool, Int)
comprobarFilasColumnas s n i
    | i > nrows s = (False, 0)
    | r == n || c == n = (True, 1)
    | r == -n || c == -n = (True, -1)
    | otherwise = comprobarFilasColumnas s n (i + 1)
    where
        r = sum (getRow i s)
        c = sum (getCol i s)



-- Generar todas las diagonales posibles de tamaño n
generarDiagonales :: Estado -> Int -> Int -> [[Int]]
generarDiagonales s n size =
    let
        -- Diagonales descendentes (de arriba a la izquierda hacia abajo a la derecha)
        diagonalesDescendentes =
            [ [getElem (i + j + 1) (j + 1) s | j <- [0..n-1]]
            | i <- [0..size - n] ]  -- Asegurarse de no sobrepasar los límites de la matriz

        -- Diagonales ascendentes (de abajo a la izquierda hacia arriba a la derecha)
        diagonalesAscendentes =
            [ [getElem (i - j + 1) (j + 1) s | j <- [0..n-1]]
            | i <- [n-1..size-1] ]  -- Asegurarse de no sobrepasar los límites de la matriz

        -- Diagonales descendentes (de izquierda a derecha, empezando por las primeras columnas)
        diagonalesDescendentes2 =
            [ [getElem (i + j + 1) (j + i + 1) s | j <- [0..n-1]]
            | i <- [0..size - n] ] 

        -- Diagonales ascendentes (de derecha a izquierda, empezando por las últimas columnas)
        diagonalesAscendentes2 =
            [ [getElem (i - j + 1) (size - j) s | j <- [0..n-1]]
            | i <- [n-1..size-1] ]
        
        -- Diagonales descendentes adicionales (de arriba hacia abajo, de derecha a izquierda)
        diagonalesDescendentes3 =
            [ [getElem (i + j + 1) (size - j) s | j <- [0..n-1]]
            | i <- [0..size - n] ]

        -- Diagonales ascendentes adicionales (de abajo hacia arriba, de izquierda a derecha)
        diagonalesAscendentes3 =
            [ [getElem (i - j + 1) (n - j) s | j <- [0..n-1]]
            | i <- [n-1..size-1] ]
    in diagonalesDescendentes ++ diagonalesAscendentes ++ diagonalesDescendentes2 ++ diagonalesAscendentes2 ++ diagonalesDescendentes3 ++ diagonalesAscendentes3

-- Comprobar todas las diagonales (descendentes y ascendentes)
comprobarDiagonales :: Estado -> Int -> (Bool, Int)
comprobarDiagonales s n = 
    let size = nrows s
        allDiags = generarDiagonales s n size
        -- Depuración: Imprimir las diagonales y sus sumas
        diagSums = map sum allDiags
        _ = print diagSums
    in case filter (\diag -> sum diag == n || sum diag == -n) allDiags of
        [] -> (False, 0)  -- No se encontró ninguna diagonal ganadora
        (d:_) -> (True, if sum d == n then 1 else -1)




-- Función que genera los hijos de un estado
hijos :: Estado -> [Estado]
hijos s = concatMap (\x -> hijosAux x s) [0..(nrows s * ncols s - 1)]

-- Función auxiliar para generar los hijos de un estado en la posición i
hijosAux :: Int -> Estado -> [Estado]
hijosAux i s
    | e == 0 = [fromList (nrows s) (ncols s) (replaceAt i (verTurno s) xs)]
    | otherwise = []
    where
        e = (toList s) !! i
        xs = toList s

-- Función para determinar el turno
verTurno :: Estado -> Int
verTurno s = if pos == neg then 1 else -1
    where
        pos = length (filter (==1) (toList s))
        neg = length (filter (==(-1)) (toList s))

-- Reemplazar el valor en la posición i de la lista xs
replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs











estado1 :: Estado
estado1 = fromList 7 8 [-1, -1, -1, -1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0]  -- Jugador -1 gana en la primera fila (4 en raya)

estado2 :: Estado
estado2 = fromList 7 8 [0, 0, 1, 0, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0]  -- Jugador 1 gana (4 en raya en la columna central)

estado3 :: Estado
estado3 = fromList 7 8 [0, 0, 0, 1, 0, 0, 0, 1,
                       0, 0, 1, 0, 0, 0, 1, 1,
                       0, 1, 0, 0, 0, 1, 0, 1,
                       0, 0, 0, 0, 1, 0, 0, 1,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0]  -- Jugador 1 gana en la diagonal descendente (4 en raya)

estado4 :: Estado
estado4 = fromList 7 8 [1, 1, 1, 1, 0, 0, 0, 0,
                       -1, -1, -1, -1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0]  -- Tablero incompleto

estado5 :: Estado
estado5 = fromList 7 8 [0, 0, 1, -1, 0, 0, 0, 0,
                       0, 0, -1, 0, 0, 0, 0, 0,
                       0, -1, 0, 0, 0, 0, 0, 0,
                       -1, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0]  -- Tablero incompleto

estado6 :: Estado
estado6 = fromList 7 8 [-1, 0, 0, 0, 0, 0, 0, 0,
                       0, -1, 0, 0, 0, 0, 0, 0,
                       0, 0, -1, 0, 0, 0, 0, 0,
                       0, 0, 0, -1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0]  -- Jugador 1 gana en la diagonal inversa (4 en raya)

-- Probar las diferentes situaciones con esEstadoFinal
testEstados :: IO ()
testEstados = do
    -- Probar estado1 (se espera (True, -1))
    print $ esEstadoFinal estado1 4  -- (True, -1)
    
    -- Probar estado2 (se espera (True, 1))
    print $ esEstadoFinal estado2 4  -- (True, 1)

    -- Probar estado3 (se espera (True, 1))
    print $ esEstadoFinal estado3 4  -- (True, 1)

    -- Probar estado4 (se espera (False, 0))
    print $ esEstadoFinal estado4 4  -- (False, 0) (Juego incompleto)

    -- Probar estado5 (se espera (False, 0))
    print $ esEstadoFinal estado5 4  -- (False, 0) (Juego incompleto)

    -- Probar estado6 (se espera (True, -1))
    print $ esEstadoFinal estado6 4  -- (True, -1) (Jugador -1 gana en la diagonal inversa)








estado12 :: Estado
estado12 = fromList 5 5 [0, -1, -1, -1, 1,
                       0, 0, 0, 0, 1,
                       0, 0, 0, 0, 1,
                       0, 0, 0, 0, 1,
                       0, 0, 0, 0, 0]  -- Jugador -1 gana en la primera fila (4 en raya)

estado22 :: Estado
estado22 = fromList 5 5 [0, 0, 1, 0, 0,
                       0, 0, 1, 0, 0,
                       0, 0, 1, 0, 0,
                       0, 0, 1, 0, 0,
                       0, 0, 0, 0, 0]  -- Jugador 1 gana (4 en raya)

estado32 :: Estado
estado32 = fromList 5 5 [0, 0, 0, 0, 0,
                       0, 0, 0, 0, 1,
                       0, 0, 0, 1, 0,
                       0, 0, 1, 0, 0,
                       0, 1, 0, 0, 0]  -- Jugador 1 gana en la diagonal (4 en raya)

estado42 :: Estado
estado42 = fromList 5 5 [1, 1, 1, 1, 0,
                       -1, -1, -1, -1, 0,
                       0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0]  -- Tablero incompleto

estado52 :: Estado
estado52 = fromList 5 5 [0, 0, 1, -1, 0,
                       0, 0, -1, 0, 0,
                       0, -1, 0, 0, 0,
                       -1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0]  -- Tablero incompleto

estado62 :: Estado
estado62 = fromList 5 5 [-1, 0, 0, 0, 0,
                       0, -1, 0, 0, 0,
                       0, 0, -1, 0, 0,
                       0, 0, 0, -1, 0,
                       0, 0, 0, 0, 0]  -- Jugador 1 gana en la diagonal inversa (4 en raya)

-- Probar las diferentes situaciones con esEstadoFinal
testEstados2 :: IO ()
testEstados2 = do
    -- Probar estado1 (se espera (True, 1))
    print $ esEstadoFinal estado12 4  -- (True, 1)
    
    -- Probar estado2 (se espera (True, 1))
    print $ esEstadoFinal estado22 4  -- (True, 1)

    -- Probar estado3 (se espera (True, 1))
    print $ esEstadoFinal estado32 4  -- (True, 1)

    -- Probar estado4 (se espera (True, 0))
    print $ esEstadoFinal estado42 4  -- (False, 0) (Juego incompleto)

    -- Probar estado5 (se espera (False, 0))
    print $ esEstadoFinal estado52 4  -- (False, 0) (Juego incompleto)

    -- Probar estado6 (se espera (True, -1))
    print $ esEstadoFinal estado62 4  -- (True, -1) (Jugador -1 gana en la diagonal inversa) pero para un 7*8 y un 4 en rayas


















-- Estado para un tablero de 3x3
estado11 :: Estado
estado11 = fromList 3 3 [-1, -1, -1,
                       0,  0,  0,
                       0,  0,  0]  -- Jugador -1 gana en la primera fila (3 en raya)

estado21 :: Estado
estado21 = fromList 3 3 [0, 0, 1,
                       0, 0, 1,
                       0, 0, 1]  -- Jugador 1 gana (3 en raya en la columna central)

estado31 :: Estado
estado31 = fromList 3 3 [0, 0, 1,
                       0, 1, 0,
                       1, 0, 0]  -- Jugador 1 gana en la diagonal descendente (3 en raya)

estado41 :: Estado
estado41 = fromList 3 3 [1, 1, 1,
                       -1, -1, -1,
                        0,  0,  0]  -- Tablero incompleto (jugadores aún no ganan)

estado51 :: Estado
estado51 = fromList 3 3 [-1, 1, 1,
                       1, 1, -1,
                       -1, -1, 1]  -- Tablero incompleto, nadie gana aún

estado61 :: Estado
estado61 = fromList 3 3 [-1, 0, 0,
                       0, -1, 0,
                       0,  0, -1]  -- Jugador -1 gana en la diagonal inversa (3 en raya)

-- Probar las diferentes situaciones con esEstadoFinal
testEstados1 :: IO ()
testEstados1 = do
    -- Probar estado1 (se espera (True, -1))
    print $ esEstadoFinal estado11 3  -- (True, -1)
    
    -- Probar estado2 (se espera (True, 1))
    print $ esEstadoFinal estado21 3  -- (True, 1)

    -- Probar estado3 (se espera (True, 1))
    print $ esEstadoFinal estado31 3  -- (True, 1)

    -- Probar estado4 (se espera (False, 0))
    print $ esEstadoFinal estado41 3  -- (False, 0) (Juego incompleto)

    -- Probar estado5 (se espera (False, 0))
    print $ esEstadoFinal estado51 3  -- (False, 0) (Juego incompleto)

    -- Probar estado6 (se espera (True, -1))
    print $ esEstadoFinal estado61 3  -- (True, -1) (Jugador -1 gana en la diagonal inversa)


