module TicTacToeProblemPr
(
    Estado,
    estadoInicial,
    esEstadoFinal,
    hijos
) where

import qualified Data.Matrix as M
import Data.List (transpose)

type Estado = M.Matrix Int

estadoInicial :: Int -> Estado
estadoInicial n = M.zero n n

esEstadoFinal :: Estado -> (Bool, Int)
esEstadoFinal estado
  | hayGanador estado = (True, ganador estado)  -- Si hay un ganador, se retorna True y el jugador ganador
  | esEmpate estado   = (True, 0)              -- Si hay un empate, se retorna True y empate
  | otherwise         = (False, 0)            -- Si no hay fin de juego, se retorna False

hayGanador :: Estado -> Bool
hayGanador estado = any (== n) filas || any (== n) columnas || any (== n) diagonales ||
                    any (== (-n)) filas || any (== (-n)) columnas || any (== (-n)) diagonales
  where
    n = M.nrows estado
    filas = map sum (M.toLists estado)
    columnas = map sum (M.toLists (M.transpose estado))
    diagonales = [sum [M.getElem i i estado | i <- [1..n]], sum [M.getElem i (n - i + 1) estado | i <- [1..n]]]

ganador :: Estado -> Int
ganador estado
  | any (== n) filas         = 1
  | any (== n) columnas      = 1
  | any (== n) diagonales    = 1
  | any (== (-n)) filas      = -1
  | any (== (-n)) columnas   = -1
  | any (== (-n)) diagonales = -1
  | otherwise                = 0
  where
    n = M.nrows estado
    filas = map sum (M.toLists estado)
    columnas = map sum (M.toLists (M.transpose estado))
    diagonales = [sum [M.getElem i i estado | i <- [1..n]], sum [M.getElem i (n - i + 1) estado | i <- [1..n]]]

esEmpate :: Estado -> Bool
esEmpate s = all (/= 0) (M.toList s)

hijos :: Estado -> [Estado]
hijos s = [M.setElem (verTurno s) (r, c) s | r <- [1..M.nrows s], c <- [1..M.ncols s], M.getElem r c s == 0]

verTurno :: Estado -> Int
verTurno s = if pos == neg then 1 else -1
  where
    pos = length (filter (== 1) (M.toList s))
    neg = length (filter (== (-1)) (M.toList s))
