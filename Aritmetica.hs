module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
criba :: Integer -> Set Integer
criba _ = []

--(3)
-- Dado un entero, devuelve otro, de manera tal que ambos sean coprimos.
coprimoCon:: Integer -> Integer
coprimoCon x = primerCoprimoDesde n (n - 2)
     where n = abs(x)

--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

{- Función mcd
 - Utiliza el Algoritmo de Euclides (no extendido) para hallar el
 - Maximo Común Divisor entre dos Integers dados
 -}
mcd ::  Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

{- Funcion primerCoprimoDesde
 - Recibe dos enteros a b, y devuelve el primer entero coprimo con a,
 - menor que b.
 -}
primerCoprimoDesde :: Integer -> Integer -> Integer
primerCoprimoDesde _ 0 = 0
primerCoprimoDesde a b | mcd a b == 1 = b
                       | otherwise = primerCoprimoDesde a (b - 1) 
