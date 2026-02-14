-- Parte A. Recursión sobre Números
-- Ejercicio 1. Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- Ejercicio 2. Modulo n
modulo :: Integer -> Integer -> Integer
modulo 0 _ = error "División por cero"
modulo n m
  | m < n = m
  | otherwise = modulo n (m - n)

-- Ejercicio 3. MCD
mcd :: Integer -> Integer -> Integer
mcd 0 b = b
mcd a 0 = a
mcd a b = mcd b (modulo b a)
