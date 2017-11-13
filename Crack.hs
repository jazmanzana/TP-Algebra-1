module Crack where
import Catedra
import Aritmetica
import RSA



--(8)
{- Intenta factorizar el `n` de la clave pública para averiguar `p` y `q`
 - En base a eso, calcula φ (ya que φ = (p - 1) · (q - 1)
 - Finalmente, como `d` es el inverso de `e` módulo φ, calcula el
 - inverso multiplicativo de `e` módulo φ y lo usa para construir la clave
 - privada
 -}
romper :: Clpub -> Clpri
romper cPub = (inversoMultiplicativo e phi, n)
            where phi = (p - 1) * (q - 1)
                  p = fst (head (factorizarEnteros n))
                  q = snd (head (factorizarEnteros n))
                  e = fst cPub
                  n = snd cPub

--(9)
{- Dada una clave pública y una secuencia de enteros, intenta decodificarla
 - secuencia original.
 -}
espia :: Clpub -> Cifrado -> Mensaje
espia cPub mensajeCif = decodificador (romper cPub) mensajeCif

{- Dado un número entero mayor a `n`, devuelve un par de números (p, e),
 - tales que p · e = n
 -}
factorizarEnteros :: Integer -> Set (Integer, Integer)
factorizarEnteros n = [(pollard n, div n (pollard n))]

factorizarEnterosAux :: [Integer] -> Integer -> [Integer]
factorizarEnterosAux _ 0 = []
factorizarEnterosAux [] _ = []
factorizarEnterosAux primos n | mod n (head primos) == 0 = (head primos) : factorizarEnterosAux primos (div n (head primos))
                              | otherwise = factorizarEnterosAux (tail primos) n


{- Dado un número entero `n`, devuelve otro número entero `d`, tal que
 - `d` es factor primo de `n`
 -}
pollard :: Integer -> Integer
pollard n = pollardAux n x y 1
          where x = (g_random 2 n)
                y = (g_random (g_random 2 n) n)
                d = (mcd (abs (x - y)) n)

pollardAux :: Integer -> Integer -> Integer -> Integer -> Integer
pollardAux n x y d | d == 1 = pollardAux n (g_random x n) (g_random (g_random y n) n) (mcd (abs (x - y)) n)
                   | d == n = 0
                   | otherwise = d

{- Función pseudoaleatoria
 -}
g_random :: Integer -> Integer -> Integer
g_random x n = mod (x^2 + 1) n