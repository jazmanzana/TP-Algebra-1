module RSA where
import Catedra
import Aritmetica


--(3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        (e, d) = moduloPhi ((p-1)*(q-1)) ((p-1)*(q-1))

-- d es el inverso de e modulo phi
moduloPhi :: Integer -> Integer -> Integer -> (Integer, Integer)
moduloPhi 0 phi = (0, inversoMultiplicativo 0 phi) -- revisar caso base
moduloPhi e phi | esCongruenteAUno (e* (inversoMultiplicativo e phi)) phi = (e, (inversoMultiplicativo e phi))
                | otherwise = moduloPhi (e-1) phi

esCongruenteAUno :: Integer -> Integer -> Bool
esCongruenteAUno a phi | a mod phi == 1 mod phi = True
                       | otherwise = False


--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador _ _ = []

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador _ _ = ""
