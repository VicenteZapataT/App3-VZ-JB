import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Read (readMaybe)

type Pos = (Int, Int)           -- Representa una posición (fila, columna) en la matriz
type Camino = [Pos]             -- Representa un camino como una lista de posiciones
type Matriz = [[Int]]           -- Representa la matriz de enteros (el "bosque")
type Energia = Int              -- Representa la energía del mago
type Cache = Map.Map (Pos, Energia) (Maybe (Camino, Energia)) -- Cache para memoización

-- Devuelve el valor de la celda en la posición dada de la matriz
valorCelda :: Matriz -> Pos -> Int
valorCelda matriz (x, y) = (matriz !! x) !! y

-- Devuelve la dimensión de la matriz (asume matriz cuadrada)
dimMatriz :: Matriz -> Int
dimMatriz = length

-- Verifica si una posición es válida dentro de la matriz
esValida :: Int -> Pos -> Bool
esValida n (x, y) = x >= 0 && y >= 0 && x < n && y < n

-- Devuelve el costo adicional de una celda (por ejemplo, -3 si es 0)
costoCelda :: Matriz -> Pos -> Int
costoCelda matriz pos
  | valorCelda matriz pos == 0 = -3
  | otherwise = 0

-- Lista de movimientos posibles y su costo extra asociado
movimientos :: [ (Pos -> Pos, Int) ]
movimientos =
  [ (\(x,y) -> (x, y+1), 0)      -- Derecha
  , (\(x,y) -> (x+1, y), 0)      -- Abajo
  , (\(x,y) -> (x+1, y+1), 2)    -- Diagonal abajo-derecha (costo extra 2)
  , (\(x,y) -> (x, y-1), 0)      -- Izquierda
  , (\(x,y) -> (x-1, y), 0)      -- Arriba
  ]
