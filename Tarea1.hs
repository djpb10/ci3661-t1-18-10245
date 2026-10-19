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
-- Transformamos las componentes de una hora en formato correcto
-- enteros con los cuales poder hacer operaciones 

leerHora :: String -> Either String (Int, Int)
leerHora [d1, d2, ':', m1, m2]
  | not ( isDigit d1 && isDigit d2 && isDigit m1 && isDigit m2 ) = Left "formato inválido"
  | digitToInt d1 * 10 + digitToInt d2 > 23                      = Left "hora fuera de rango"
  | digitToInt m1 * 10 + digitToInt d2 > 59                      = Left "minutos fuera de rango"
  | otherwise  = Right (digitToInt d1 * 10 + digitToInt d2, digitToInt m1 * 10 + digitToInt m2 )

-- Dado que haskel revisa en orden y usa la primera que calse
-- tenemos este caso donde el formato no concuerda de ninguna manera
leerHora _ = Left "formato inválido"


-- Anidamos case para lograr el flujo de verificacion 
-- revisamos cada hora para luego realizar las comparaciones
-- verificamos si una hora pertenece a otro dia y luego 
-- realizamos el calculo de los minutos transcurridos
duracion :: String -> String -> Either String Int
duracion s1 s2 =
  case leerHora s1 of
    Left error1 -> Left error1
    Right (h1, m1) ->
      case leerHora s2 of
        Left error2 -> Left error2
        Right (h2, m2) ->
          if (h2 * 60 + m2) >= (h1 * 60 + m1)
            then Right ((h2 * 60 + m2) - (h1 * 60 + m1))
            else Right ((h2 * 60 + m2) + 1440 - (h1 * 60 + m1))

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
-- Usamos foldr para ir extrayendo los elementos uno a uno
-- Una vez teniendo la estructura completa la funcion lambda se encarga
-- se sumar los numeros que cumplen la condicion o dejarla igual.
-- Si no consigue alguno, la cuenta es 0.0 y retorna nothing, Just en caso contrario.

promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs =
  let (suma, cuenta) = foldr (\x (s, c) -> if x > umbral then (s + x, c + 1) else (s, c)) (0.0, 0.0) xs
  in if cuenta == 0.0
    then Nothing
    else Just (suma / cuenta)

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
