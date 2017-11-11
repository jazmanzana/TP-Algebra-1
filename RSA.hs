module RSA where
import Catedra
import Aritmetica


-- (3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        (e, d) = moduloPhi ((p-1)*(q-1)) ((p-1)*(q-1)) -- llamo con phi dos veces y una lo voy descontando

-- d es el inverso de e modulo phi = calculo d con inversoMultiplicativo
-- e es un numero entre 0 y phi que cumpla con la ecuacion de congruencia dada
moduloPhi :: Integer -> Integer -> (Integer, Integer)
moduloPhi 0 phi = (0, inversoMultiplicativo 0 phi) -- revisar caso base: tiene que ser con e 0 pero que devuelvo en d?
moduloPhi e phi | esCongruenteAUno (e* (inversoMultiplicativo e phi)) phi = (e, (inversoMultiplicativo e phi)) --if True, devuelve e y d
                | otherwise = moduloPhi (e-1) phi --if False, probamos con e mas pequenio

-- llamo a esCongruenteAUno con e*d y phi (que es el modulo)
esCongruenteAUno :: Integer -> Integer -> Bool
esCongruenteAUno a phi | mod a phi == mod 1 phi = True
                       | otherwise = False


--(6)
codificador :: Clpub -> Mensaje -> Cifrado
codificador _ _ = []

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador _ _ = ""
