module RSA where
import Catedra
import Aritmetica


-- (3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        (e, d) = calcular_e_d ((p-1)*(q-1)) ((p-1)*(q-1)) -- llamo con phi dos veces, una es mi e que le voy restando 1


{- Recibe dos números, `f` y φ y devuelve los valores `e`, `d` utilizados
 - por RSA
 -}
calcular_e_d :: Integer -> Integer -> (Integer, Integer)
calcular_e_d 0 _ = (0, 0)
calcular_e_d f phi | abs (inversoMultiplicativo f phi) /= 1 = (f, inversoMultiplicativo f phi)
                   | otherwise = calcular_e_d (f - 1) phi



--(6)
-- Dada la clave pública y una secuencia de caracteres, la codifica y devuelve la secuencia obtenida.
codificador :: ClPub -> Mensaje -> Cifrado
codificador kPublic mensaje | fst(mcdExt snd(kPublic) (aEnteros mensaje)) == 1 = convertirLista (aEnteros mensaje) kPublic 
							| otherwise = -(aEnteros mensaje)

--(7)
-- Dados una clave privada y un mensaje cifrado devuelve un mensaje
-- decodificado
decodificador :: ClPri -> Cifrado -> Mensaje
decodificador kPriv mCifrado | mCifrado >= 0 = revertirLista mCifrado ClPri
							 | otherwise = aChars (- mCifrado) 

							 
-- funcion auxiliar que recibe el mensaje original y devuelve el mensaje cifrado para el primer caso 

convertirLista :: [Integer] -> ClPub -> [Integer]
convertirLista [] clave = []
convertirLista lista clave =  modExp (head lista) fst(clave) snd(clave) : convertirLista (tail lista) clave

-- funcion auxiliar que recibe el mensaje cifrado y devuelve el mensaje original para el primer caso

revertirLista :: Cifrado -> ClPri -> Mensaje
revertirLista [] kPriv = []
revertirLista mCifrado kPriv = modExp (head mCifrado) fst(kPriv) snd(kPriv) : convertirLista (tail mCifrado) kPriv