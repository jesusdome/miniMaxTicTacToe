import MiniMaxTAD
import Data.Char
import System.IO
import TicTacToeProblemPr
import Data.Matrix
import Data.List (intercalate)

main :: IO ()
main = do
    putStrLn "Bienvenido a N en Raya"
    n <- insertarDigito "¿De qué tamaño desea jugar? (Por ejemplo, 3 para 3x3): "
    let estadoInicial = matrix n n (const 0) -- Crear un tablero vacío de tamaño dinámico
    menu n estadoInicial

menu :: Int -> Estado -> IO ()
menu n estadoInicial = do
    cabeceraMenu
    putStr "\n"
    putStrLn "¿Qué acción desea realizar?"
    putStr "\n"
    putStrLn "          1) Jugar una nueva partida contra el ordenador"
    putStrLn "          2) Encontrar la mejor jugada"
    putStrLn "          3) Jugar una nueva partida multijugador"
    putStr "\n"
    m <- insertarDigito "Acción a realizar: "
    filtrarEleccionMenu n estadoInicial m

cabeceraMenu :: IO ()
cabeceraMenu = do
    putStrLn "#################################################################"
    putStrLn "#                              MENU                             #"
    putStrLn "#################################################################"

insertarDigito :: String -> IO Int
insertarDigito s = do
    putStr s
    hFlush stdout
    r <- getLine
    if all isDigit r && not (null r)
        then return (read r)
        else do
            putStrLn "Error: Valor incorrecto"
            insertarDigito s

filtrarEleccionMenu :: Int -> Estado -> Int -> IO ()
filtrarEleccionMenu n estadoInicial i
    | i == 1 = jugarContraOrdenador n estadoInicial
    | otherwise = do
        putStrLn "Error: Valor incorrecto"
        x <- insertarDigito "Acción a realizar: "
        filtrarEleccionMenu n estadoInicial x

representarTablero :: Estado -> IO ()
representarTablero s = do
    let n = nrows s
    mapM_ (\i -> do
        putStrLn $ r i
        if i < n then putStrLn l else return ()
        ) [1..n]
  where
    r i = " " ++ intercalate " | " (map (\j -> e i j) [1..nrows s])
    e i j = conversionFichas (getElem i j s)
    l = intercalate "+" (replicate (ncols s) (replicate 3 '-'))

conversionFichas :: Int -> String
conversionFichas i
    | i == 1    = "X"
    | i == -1   = "O"
    | otherwise = " "

jugarContraOrdenador :: Int -> Estado -> IO ()
jugarContraOrdenador n estadoInicial = do
    putStrLn "\n"
    jugada n 1 estadoInicial True (emptyTree estadoInicial)

jugada :: Int -> Int -> Estado -> Bool -> MiniMaxTree Estado -> IO ()
jugada n jugador estado esPrimera minimaxT = do
    putStrLn "\n"
    putStrLn $ "Turno del jugador " ++ conversionFichas (siguienteJugador jugador)
    putStrLn "\n"
    representarTablero estado
    putStrLn "\n"
    if fst (esEstadoFinal estado)
    then if snd (esEstadoFinal estado) /= 0
            then putStrLn $ "¡¡¡¡¡¡ HA GANADO EL JUGADOR " ++ conversionFichas (siguienteJugador jugador) ++ "!!!!!!"
            else putStrLn "Habéis empatado :("
        else if jugador == 1
            then jugadaHumano n estado esPrimera minimaxT
            else jugadaOrdenador n estado minimaxT

jugadaHumano :: Int -> Estado -> Bool -> MiniMaxTree Estado -> IO ()
jugadaHumano n s esPrimera minimaxT = do
    s2 <- modificaTablero n s 1
    if esPrimera
        then jugada n (-1) s2 False (crearArbolMM s2)
        else jugada n (-1) s2 False minimaxT

jugadaOrdenador :: Int -> Estado -> MiniMaxTree Estado -> IO ()
jugadaOrdenador n s mmt = do
    let arbolMM = if mmt == emptyTree s then crearArbolMM s else mmt
    jugada n 1 (mejorJugada arbolMM s False) False arbolMM


crearArbolMM :: Estado -> MiniMaxTree Estado
crearArbolMM s = generaArbolMiniMax arbolE False esEstadoFinal
    where arbolE = generarArbolDeEstados s hijos esEstadoFinal

modificaTablero :: Int -> Estado -> Int -> IO Estado
modificaTablero n s j = do
    r <- insertarDigito "Fila que desea marcar: "
    c <- insertarDigito "Columna que desea marcar: "
    if elem r [1..n] && elem c [1..n] && (getElem r c s) == 0
        then return $ setElem j (r, c) s
        else do
            putStrLn "Error: No se puede escribir en esta casilla"
            modificaTablero n s j

siguienteJugador :: Int -> Int
siguienteJugador j = 0 - j
