import Text.XHtml (rev)
{-
    Práctica 0 - Introducción a Haskell de Lógica Computacional
    Profesor: Dr. Marco Vladimir Lemus Yañez
    Ayudante: Dr. Fernando Cruz Pineda
    Fecha: 14/02/2026 

    Inetgrantes del equipo:
    - Becerra Valencia César
    - Cortes Nava José Luis
    - Díaz Anavia Javier Omar
-}

-- PARTE A: RECURSIÓN SOBRE NÚMEROS



--Ejercicio 1. Fibonacci
{-
  Calcula el n-ésimo número de la secuencia de Fibonacci.
  Usa casos base para 0 y 1. Para n > 1, suma las dos llamadas recursivas anteriores.
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--Ejercicio 2. Módulo n
{-
  La funcion calcula el residuo de la division m / n
  Resta recursivamenmte un divisor (n) del dividendo (m) hasta que
  su resultado sea menor que el divisor.
-}
modulo :: Integer -> Integer -> Integer
modulo 0 _ = error "No se puede dividir entre cero"
modulo n m
  | m < n = m
  | otherwise = modulo n (m - n)


-- Ejercicio 3. MCD
{-
  La funcion calcula el maximo comun divisor (MCD) de 
  dos numeros enteros, aplicando el algoritmo de Euclides.
  Utilando llamadas recursivas pasando el divisor y el 
  residuo (que se calcula con la funcion anterior modulo)
  hasta que el residuo sea exactamente 0.
-}
mcd :: Integer -> Integer -> Integer
mcd 0 b = b
mcd a 0 = a
mcd a b = mcd b (modulo b a)


-- PARTE B: RECURSIÓN SOBRE LISTAS


-- Ejercicio 4. Reversa de lista
{-  
  La funcion reversa invierte los elementos de una Lisa.
  Para una lista vacia, devuelve una lista vacia.
  Para una lista no vacia, separa la cabeza (x) de
  la cola (xs), despues invierte la cola de 
  forma recursiva para finalizar concatenando
  la cabeza al final.
-}
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = concatenar (reversa xs) [x]

{-
  La funcion concatenar une dos listas.
  Para una lista vacia, devuelve la otra lista.
  Para una lista no vacia, separa la cabeza (x) de
  la cola (xs), despues concatena la cabeza a la
  concatenacion recursiva de la cola con la otra lista.
-}
concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys


-- Ejercicio 5. Máximo de elementos
{-
  La funcion encuentra el valor Maximo de una lista de enteros.
  Para una lista vacia, lanza un error.
  Para una lista con un solo elemento, devuelve ese elemento.
  Para una lista con mas de un elemento, separa la cabeza (x) de
  la cola (xs), despues encuentra el maximo de la cola de forma
  recursiva para finalizar comparando el valor de la cabeza con el
  maximo de la cola y devolviendo el mayor de los dos.
-}
maximo :: [Integer] -> Integer
maximo [] = error "La lista no puede estar vacía"
maximo [x] = x
maximo (x:xs) =
  let maxRest = maximo xs
  in if x > maxRest then x else maxRest

-- Ejercicio 6. Filtrar lista
{-
  La funcion pares filtra los elementos pares de una lista de enteros.
  Para una lista vacia, devuelve una lista vacia.
  Para una lista no vacia, separa la cabeza (x) de la cola (xs), despues
  verifica si la cabeza es par utilizando la funcion modulo. Si es par,
  incluye la cabeza en el resultado concatenandola con el resultado recursivo
  de filtrar la cola. Si no es par, simplemente devuelve el resultado recursivo
  de filtrar la cola sin incluir la cabeza.
-}
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
  | modulo 2 x == 0 = x : pares xs
  | otherwise = pares xs

-- Ejercicio 7. Contar Ocurrencias
{-
  La funcion contar cuenta el numero de veces que un entero n aparece en una lista de enteros.
  Para una lista vacia, devuelve 0.
  Para una lista no vacia, separa la cabeza (x) de la cola (xs), despues
  verifica si la cabeza es igual a n. Si es igual, incrementa el contador en 1 y continua
  contando en la cola. Si no es igual, simplemente continua contando en la cola sin incrementar
  el contador.
-}
contar :: Integer -> [Integer] -> Integer
contar _ [] = 0
contar n (x:xs)
  | x == n = 1 + contar n xs
  | otherwise = contar n xs

-- PARTE C: TIPOS ALGÉBRAICOS

-- Ejercicio 8. Definición de números naturales
data Nat = Z | S Nat deriving Show

-- Ejercicio 9. Conversión a entero
{-
  La funcion convertir transforma un numero natural definido por el tipo Nat a un entero.
  Para el caso base Z, devuelve 0. Para el caso recursivo S n, devuelve 1 mas el resultado
  de convertir n, lo que representa la cantidad de sucesores aplicados a Z.
-}
convertir :: Nat -> Integer
convertir Z = 0
convertir (S n) = 1 + convertir n

-- Ejercicio 10. Multiplicación sobre Nat
{-
  La funcion mult calcula el producto de dos numeros naturales definidos por el tipo Nat.
  Para el caso base Z, devuelve Z. Para el caso recursivo S n, devuelve la suma del segundo
  numero (m) con el resultado de multiplicar n por m, lo que representa la cantidad de veces
  que se suma m a si mismo para obtener el producto.
-}
mult :: Nat -> Nat -> Nat
mult Z _ = Z
mult _ Z = Z
mult (S n) m = sumarAux m (mult n m)

{-
  La funcion sumarAux es una función auxiliar que realiza la suma de dos números naturales definidos 
  por el tipo Nat.
  
  Para el caso base Z, devuelve el segundo número (m). Para el caso recursivo S n, devuelve el 
  sucesor de la suma de n con m, lo que representa la cantidad de veces que se suma m a si mismo 
  para obtener la suma total.
-}
sumarAux :: Nat -> Nat -> Nat
sumarAux Z m = m
sumarAux (S n) m = S (sumarAux n m)
