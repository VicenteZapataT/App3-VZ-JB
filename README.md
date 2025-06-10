
# App 3 – El Bosque de las Runas Mágicas
**Curso:** TICS200 - Paradigma Funcional  
**Lenguaje:** Haskell

- **José Pablo Bernal** – josbernal@alumnos.uai.cl  
- **Vicente Zapata** – vizapata@alumnos.uai.cl

## Descripción

Este programa resuelve el desafío de encontrar el **camino óptimo** para un mago dentro de un bosque representado como una matriz de runas, buscando maximizar su energía final. Se implementó completamente en el **paradigma funcional**, utilizando **Haskell**, con funciones puras, recursión y memoización con `Data.Map`.

El mago puede moverse en distintas direcciones (derecha, abajo, diagonal, izquierda, arriba), con restricciones de energía y penalizaciones especiales por trampas.

## Cómo compilar y ejecutar

### 1. Ejecutar el programa

runghc juegoMago.hs

Se pedirá ingresar:

1. La matriz del bosque (ejemplo: `[[2, -3, 1], [-5, 4, 0], [1, 3, -2]]`)  
2. La energía inicial del mago (ejemplo: `12`)

El programa devolverá el camino óptimo y la energía final alcanzada.
