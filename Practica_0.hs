{-
    Práctica 0 - Introducción a Haskell de Lógica Computacional
    Profesor: Dr. Marco Vladimir Lemus Yañez
    Ayudante: Dr. Fernando Cruz Pineda
    Fecha: 14/02/2026 

    Inetgrantes del equipo:
    - Becerra Valencia César
    - Cortez Nava José Luis
    - Díaz Anavia Javier Omar
-}

-- PARTE A: RECURSIÓN SOBRE NÚMEROS

-- Ejercicio 1. Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Ejercicio 2. Módulo n
modulo :: Integer -> Integer -> Integer
modulo 0 _ = error "No se puede dividir entre cero"
modulo n m
  | m < n = m
  | otherwise = modulo n (m - n)

-- Ejercicio 3. MCD
mcd :: Integer -> Integer -> Integer
mcd 0 b = b
mcd a 0 = a
mcd a b = mcd b (modulo b a)

-- PARTE B: RECURSIÓN SOBRE LISTAS

-- Ejercicio 4. Reversa de lista

-- Ejercicio 5. Máximo de elementos

-- Ejercicio 6. Filtrar lista

-- Ejercicio 7. Contar Ocurrencias

-- PARTE C: TIPOS ALGÉBRAICOS

-- Ejercicio 8. Definición de números naturales

-- Ejercicio 9. Conversión a entero

-- Ejercicio 10. Multiplicación sobre Nat
