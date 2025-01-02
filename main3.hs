import MiniMaxTAD
import Data.Char
import System.IO
import TicTacToeProblemPrF
import Data.Matrix

main :: IO ()
main = do
    menu

menu :: IO ()
menu = do
    cabeceraMenu
    putStrLn "\n¿En qué tamaño de tablero quieres jugar? (Por ejemplo: 7 8)"
    putStr "Fila: "
    fila <- insertarDigito "Número de filas: "
    putStr "Columna: "
    columna <- insertarDigito "Número de columnas: "
    putStr "Número de fichas consecutivas necesarias para ganar: "
    n <- insertarDigito "Número de fichas consecutivas: "
    putStrLn "\n¿Quién jugará contra quién?"
    putStrLn "          1) Jugar una nueva partida contra el ordenador"
    putStrLn "          2) Encontrar la mejor jugada"
    putStrLn "          3) Jugar una nueva partida multijugador"
    m <- insertarDigito "Acción a realizar: "
    filtrarEleccionMenu fila columna n m

cabeceraMenu :: IO ()
cabeceraMenu = do
    putStrLn "#################################################################"
    putStrLn "#                              MENU                             #"
    putStrLn "#################################################################"

insertarDigito :: String -> IO Int
insertarDigito s = do
    putStr s
    r <- getLine
    if all isDigit r
        then return (read r)
    else do
        putStrLn "Error: Valor incorrecto"
        insertarDigito s

filtrarEleccionMenu :: Int -> Int -> Int -> Int -> IO ()
filtrarEleccionMenu filas columnas n i
    | i == 1 = jugarContraOrdenador filas columnas n
    | i == 2 = putStrLn "Encontrar la mejor jugada aún no está implementado."
    | i == 3 = putStrLn "Multijugador aún no está implementado."
    | otherwise = do
        putStrLn "Error: Valor incorrecto"
        x <- insertarDigito "Acción a realizar: "
        filtrarEleccionMenu filas columnas n x

representarTablero :: Estado -> IO ()
representarTablero s = do
    let rows = nrows s
    let cols = ncols s
    mapM_ (putStrLn . formatRow) [1..rows]
    where
        formatRow i = unwords [conversionFichas (getElem i j s) | j <- [1..ncols s]]

conversionFichas :: Int -> String
conversionFichas i 
    | i == 1 = "X"
    | i == -1 = "O"
    | otherwise = "-"

jugarContraOrdenador :: Int -> Int -> Int -> IO ()
jugarContraOrdenador filas columnas n = do
    let tablero = estadoInicial filas columnas
    putStrLn "\n"
    jugada 1 tablero True (emptyTree tablero) n

jugada :: Int -> Estado -> Bool -> MiniMaxTree Estado -> Int -> IO ()
jugada jugador estado esPrimera minimaxT n = do
    putStrLn "\n"
    putStrLn $ "Turno del jugador " ++ (conversionFichas.siguienteJugador) jugador
    putStrLn "\n"
    representarTablero estado
    putStrLn "\n"
    if fst (esEstadoFinal estado n)
        then
            if snd (esEstadoFinal estado n) /= 0
                then putStrLn $ "¡¡¡¡¡¡ HA GANADO EL JUGADOR " ++ (conversionFichas.siguienteJugador) jugador ++ "!!!!!!"
            else
                putStrLn "¡Empate!"
        else
            if jugador == 1
                then jugadaHumano estado esPrimera minimaxT n
            else
                jugadaOrdenador estado minimaxT n

{-
jugadaHumano :: Estado -> Bool -> MiniMaxTree Estado -> Int -> IO ()
jugadaHumano s esPrimera minimaxT n = do
    s2 <- modificaTablero s 1
    if esPrimera
        then jugada (-1) s2 False (crearArbolMM s2) n
    else jugada (-1) s2 False minimaxT n
-}
jugadaHumano :: Estado -> Bool -> MiniMaxTree Estado -> Int -> IO ()
jugadaHumano s esPrimera minimaxT n = do
    s2 <- modificaTablero s 1
    if esPrimera
        then jugada (-1) s2 False (crearArbolMM s2 n) n
    else jugada (-1) s2 False minimaxT n

jugadaOrdenador :: Estado -> MiniMaxTree Estado -> Int -> IO ()
jugadaOrdenador s mmt n = jugada 1 (mejorJugada mmt s False) False mmt n

{-
crearArbolMM :: Estado -> MiniMaxTree Estado
crearArbolMM s = generaArbolMiniMax arbolE False esEstadoFinal
    where arbolE = generarArbolDeEstados s hijos esEstadoFinal
-}
crearArbolMM :: Estado -> Int -> MiniMaxTree Estado
crearArbolMM s n = generaArbolMiniMax arbolE False esEstadoFinal n
    where arbolE = generarArbolDeEstados s hijos esEstadoFinal n



modificaTablero :: Estado -> Int -> IO Estado
modificaTablero s j = do
    putStrLn "Introduce las coordenadas (1-based) de la jugada."
    r <- insertarDigito "Fila que desea marcar: "
    c <- insertarDigito "Columna que desea marcar: "
    if elem r [1..nrows s] && elem c [1..ncols s] && (getElem r c s) == 0
        then return $ setElem j (r,c) s
    else do
        putStrLn "Error: No se puede escribir en esta casilla"
        modificaTablero s j

siguienteJugador :: Int -> Int
siguienteJugador j = 0 - j

