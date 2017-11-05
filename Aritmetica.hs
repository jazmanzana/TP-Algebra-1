module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
-- Dado un entero, devuelve un conjunto con todos los números primos
-- menores a él.
criba :: Integer -> Set Integer
criba 2 = []
criba n | esPrimo (n - 1) = (n - 1) : criba (n - 1)
        | otherwise = criba (n - 1)

--(3)
coprimoCon:: Integer -> Integer
coprimoCon _ = 0


--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
  
{- Función sumaDivisores - Devuelve la suma de todos los divisores de un número dado -}
sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta (abs(n)) (abs(n))

{- Función sumaDivisoresHasta
 - Realiza una suma entre los divisores de un número que sean menores
 - a otro número dado (que representa el número máximo desde el que se
 - comienzan a buscar divisores y se va reduciendo con cada ejecución)
 -}
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta dividendo divisorLimite | divisorLimite == 0 = 0
                                           | divisorLimite == 1 = 1                                           
                                           | dividendo `mod` divisorLimite == 0 = (sumaDivisoresHasta dividendo (divisorLimite - 1)) + divisorLimite
                                           | otherwise = sumaDivisoresHasta dividendo (divisorLimite - 1)
                                           
{- Función esPrimo
 - Recibe un número entero y devuelve True si es primo -}
esPrimo :: Integer -> Bool
esPrimo n = sumaDivisores (abs(n)) == abs(n) + 1
