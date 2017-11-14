{- Integrantes del grupo
 - Laur, Juan Lucas (Número de libreta: 657/17)
 - Aranda, Agustín (Número de libreta: 730/17)
 - Gómez, Mercedes Jazmín (Número de libreta: 143/11)
 -}

module Catedra where
import Data.Char

type Set a = [a]
type Clpub = (Integer, Integer) -- Clave (e, n)
type Clpri = (Integer, Integer) -- Clave (d, n)
type Cifrado = [Integer]
type Mensaje = [Char]

aEnteros :: [Char] -> [Integer]
aEnteros = map (toInteger . ord)

aChars :: [Integer] -> [Char]
aChars = map (chr . fromInteger)
