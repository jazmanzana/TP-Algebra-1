module RSA where
import Catedra
import Aritmetica


-- (3)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where n = p*q
        (e, d) = moduloPhi ((p-1)*(q-1)) ((p-1)*(q-1)) -- llamo con phi dos veces, una es mi e que le voy restando 1

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


codificador :: ClPub -> Mensaje -> Cifrado
codificador kPublic mensaje | fst(mcdExt snd(kPublic) (aEnteros mensaje)) == 1 = convertirLista (aEnteros mensaje) kPublic 
							| otherwise = -(aEnteros mensaje)

							
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