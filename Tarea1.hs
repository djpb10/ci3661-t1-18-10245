-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: David Pereira
--  Carnet: 18-10245
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo x = reconstruir x == voltear (reconstruir x)


-- Funcion para recontruir el String original
-- eliminando todo lo que no sea letra o digito
-- ademas lleva todo a minuscula

reconstruir :: String -> String
reconstruir []    = []
reconstruir (x:xs) 
  | isAlphaNum x  = toLower x : reconstruir xs
  | otherwise     = reconstruir xs

-- Funcion para voltear un String

voltear :: String -> String
voltear []     = []
voltear (x:xs) = voltear xs ++ [x]

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: Solo recorre la lista una vez porque en cada llamada
-- recursiva solo se verifica 1 termino, en la primera llamada se toma 2 veces el primero
-- para la segunda se compara con el segundo termino; sucesivamente se verifica alguna de 
-- las guardias contra un solo elemento en la lista.

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor []     = Nothing
mayorYMenor (x:xs) = Just (recorrido x x (xs))

-- Funcion para recorrer la lista comparando uno a uno los elemntos contra
-- los elementos que tomamos en un principio, solo se visita a cada elemnto
-- una unica vez porque se recuerdan los candidatos.

recorrido :: Ord a => a -> a -> [a] -> (a,a)
recorrido mayor menor []     = (mayor, menor)
recorrido mayor menor (x:xs)
  | x > mayor = recorrido x menor xs 
  | x < menor = recorrido mayor x xs
  | otherwise = recorrido mayor menor xs

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
