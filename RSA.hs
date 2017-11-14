module RSA where
import Catedra
import Aritmetica


-- (3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        (e, d) = calcular_e_d ((p-1)*(q-1)) ((p-1)*(q-1)) -- llamo con phi dos veces


{- Recibe dos números, `f` y φ y devuelve los valores `e`, `d` utilizados
 - por RSA
 -}
calcular_e_d :: Integer -> Integer -> (Integer, Integer)
calcular_e_d 0 _ = (0, 0)
calcular_e_d f phi | abs (inversoMultiplicativo f phi) /= 1 = (f, inversoMultiplicativo f phi)
                   | otherwise = calcular_e_d (f - 1) phi


--(6)
-- Dada la clave pública y una secuencia de caracteres, la codifica
-- caracter a caracter y devuelve la secuencia obtenida.
codificador :: Clpub -> Mensaje -> Cifrado
codificador cPub [] = []
codificador cPub mensajeCh | mcd m n == 1 = (modExp m e n) : codificador cPub (tail mensajeCh)
                           | otherwise = (-m) : codificador cPub (tail mensajeCh)
                           where mensajeInt = aEnteros mensajeCh
                                 m = head mensajeInt
                                 e = fst cPub
                                 n = snd cPub

--(7)
-- Dados una clave privada y un mensaje cifrado devuelve un mensaje
-- decodificado
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador cPriv [] = []
decodificador cPriv mensajeCif = aChars (decodificadorAux cPriv mensajeCif)

{- Dados una clave privada y un mensaje cifrado devuelve una lista de
 - Integers decodificados
 -}
decodificadorAux :: Clpri -> Cifrado -> [Integer]
decodificadorAux cPriv [] = []
decodificadorAux cPriv mensajeCif | c >= 0 = (modExp c d n) : decodificadorAux cPriv (tail mensajeCif)
                                  | otherwise = (-c) : decodificadorAux cPriv (tail mensajeCif)
                                  where c = head mensajeCif
                                        d = fst cPriv
                                        n = snd cPriv