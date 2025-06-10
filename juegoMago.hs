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

-- Función principal que resuelve el problema y devuelve el mejor camino y energía final
resolver :: Matriz -> Energia -> (Camino, Energia)
resolver matriz energiaInicial = fromMaybe ([], -1) resultado
  where
    n = dimMatriz matriz
    (resultado, _) = dfs Map.empty (0, 0) energiaInicial []

    -- Búsqueda en profundidad con memoización para encontrar el mejor camino
    dfs :: Cache -> Pos -> Energia -> Camino -> (Maybe (Camino, Energia), Cache)
    dfs cache pos energia camino
      | not (esValida n pos) = (Nothing, cache)         -- Posición fuera de la matriz
      | pos `elem` camino = (Nothing, cache)            -- Evita ciclos
      | otherwise =
          case Map.lookup (pos, energia) cache of       -- Consulta en cache
            Just res -> (res, cache)
            Nothing ->
              let energia' = energia + valorCelda matriz pos + costoCelda matriz pos
                  camino' = pos : camino
              in if energia' < 0                        -- Si la energía es negativa, descarta el camino
                   then (Nothing, cache)
                   else if pos == (n-1, n-1)            -- Si llegó al destino, retorna el camino y energía
                          then (Just (reverse camino', energia'), cache)
                          else
                            let siguientes = filter (\(p, _) -> esValida n p && notElem p camino') $
                                              [ (f pos, c) | (f, c) <- movimientos ]
                                (resList, cache') = dfsLista cache energia' camino' siguientes
                                mejor = if null resList then Nothing else Just (maximumBy (comparing snd) resList)
                                cacheFinal = Map.insert (pos, energia) mejor cache'
                            in (mejor, cacheFinal)

    -- Aplica dfs a una lista de posibles movimientos y acumula los caminos válidos
    dfsLista :: Cache -> Energia -> Camino -> [(Pos, Int)] -> ([(Camino, Energia)], Cache)
    dfsLista cache _ _ [] = ([], cache)
    dfsLista cache energia camino ((p, extra):rest) =
      let (res, cache') = dfs cache p (energia - extra) camino
          (resList, cache'') = dfsLista cache' energia camino rest
      in (maybe resList (:resList) res, cache'')
      
-- Lee la matriz desde la entrada estándar, validando el formato
leerMatriz :: IO Matriz
leerMatriz = do
  putStrLn "Ingrese la matriz (ejemplo: [[1,2,3],[4,5,6],[7,8,9]]):"
  input <- getLine
  case readMaybe input :: Maybe Matriz of
    Just m  -> return m
    Nothing -> do
      putStrLn "Formato inválido. Intente de nuevo."
      leerMatriz
