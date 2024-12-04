-- TO DO: Optimizar la función hijos.

import MiniMaxTAD
import Data.Char
import System.IO
import TicTacToeProblem
import Data.Matrix

main :: IO ()
main = do
    menu


menu :: IO ()
menu = do
    cabeceraMenu
    putStr "\n"
    putStrLn "¿Qué acción desea realizar?"
    putStr "\n"
    putStrLn "          1) Jugar una nueva partida contra el ordenador"
    putStrLn "          2) Encontrar la mejor jugada"
    putStrLn "          3) Jugar una nueva partida multijugador"
    putStr "\n"
    m <- insertarDigito "Acción a realizar: "
    filtrarEleccionMenu m


cabeceraMenu :: IO ()
cabeceraMenu = do
    putStrLn "#################################################################"
    putStrLn "#                              MENU                             #"
    putStrLn "#################################################################"

insertarDigito :: String -> IO Int
insertarDigito s = do
    putStr s
    r <- getChar
    if isDigit r
        then
            do
                putStr "\n"
                return (read [r])
    else
        do
            putStr "\n"
            putStrLn "Error: Valor incorrecto"
            insertarDigito s

filtrarEleccionMenu :: Int -> IO ()
filtrarEleccionMenu i
    | i == 1 = do 
            jugarContraOrdenador
    | otherwise = do
        putStrLn "Error: Valor incorrecto"
        x <- insertarDigito "Acción a realizar: "
        filtrarEleccionMenu x


representarTablero :: Estado -> IO ()

representarTablero s = do
    putStrLn $ r 1
    putStrLn l
    putStrLn $ r 2
    putStrLn l
    putStrLn $ r 3
        where
            r i = " " ++ (e i 1) ++ " | " ++ (e i 2) ++ " | " ++ (e i 3)
            e i j = conversionFichas (getElem i j s)
            l = "------------"


conversionFichas :: Int -> String
conversionFichas i 
    | i == 1 = "X"
    | i == -1 = "O"
    | otherwise = "-"

jugarContraOrdenador :: IO ()
jugarContraOrdenador = do
    putStrLn "\n"
    jugada 1 estadoInicial True (emptyTree estadoInicial)

jugada :: Int -> Estado ->  Bool -> MiniMaxTree Estado-> IO ()
jugada jugador estado esPrimera minimaxT = 
    do
        putStrLn "\n"
        putStrLn $ "Turno del jugador " ++ (conversionFichas.siguienteJugador) jugador
        putStrLn "\n"
        representarTablero estado
        putStrLn "\n"
        if (fst.esEstadoFinal) estado
            then
                if ((snd.esEstadoFinal) estado) /= 0
                    then
                        putStrLn $ "¡¡¡¡¡¡ HA GANADO EL JUGADOR " ++ (conversionFichas.siguienteJugador) jugador ++ "!!!!!!"
                else
                    putStrLn "Habeis empatado :("
        else
            if jugador == 1
                then
                    jugadaHumano estado esPrimera minimaxT
            else
                jugadaOrdenador estado minimaxT

jugadaHumano :: Estado -> Bool -> MiniMaxTree Estado -> IO ()
jugadaHumano s esPrimera minimaxT = do
    s2 <- modificaTablero s 1
    if esPrimera
        then
            jugada (-1) s2 False (crearArbolMM s2)
    else
        jugada (-1) s2 False minimaxT

jugadaOrdenador :: Estado -> MiniMaxTree Estado -> IO ()
jugadaOrdenador s mmt = jugada 1 (mejorJugada mmt s False) False mmt

crearArbolMM :: Estado -> MiniMaxTree Estado
crearArbolMM s = generaArbolMiniMax arbolE False esEstadoFinal
    where arbolE = generarArbolDeEstados s hijos esEstadoFinal

modificaTablero :: Estado -> Int -> IO Estado
modificaTablero s j = do
    r <- insertarDigito "Fila que desea marcar: "
    c <- insertarDigito "Columna que desea marcar: "
    if  elem r [1..3] && elem c [1..3] && (getElem r c s) == 0
        then
            do
                return $ setElem j (r,c) s
    else
        do
            putStrLn "Error: No se puede escribir en esta casilla"
            modificaTablero s j
    
siguienteJugador :: Int -> Int
siguienteJugador j = 0 - j