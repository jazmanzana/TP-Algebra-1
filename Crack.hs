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
                  p = fst (head (factorizarPrimos n))
                  q = snd (head (factorizarPrimos n))
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

factorizarPrimos :: Integer -> Set (Integer, Integer)
factorizarPrimos n = [(divisor_primo_mas_grande, div n divisor_primo_mas_grande)]
       where n_sqrt = floor (sqrt (fromInteger n))
             divisor_primo_mas_grande = factorizarPrimosAux n_sqrt n
             
{- Dados dos números naturales, `m` y `n`, revisa todos los enteros menores
 - a `m` y devuelve el primer entero primo, divisor de `n` que encuentre.
 - Si no encentra ninguno, devuelve 1.
 -}
 
factorizarPrimosAux :: Integer -> Integer -> Integer
factorizarPrimosAux 1 n = 1
factorizarPrimosAux m n | (esPrimo m) && (mod n m == 0) = m
                         | otherwise = factorizarPrimosAux (m-1) n
