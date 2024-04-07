letrasIntermedias :: String -> Int
letrasIntermedias string | length string < 2 = 0
                         | otherwise = length string - 2

esCapicua :: String -> Bool
esCapicua string = string == reverse string

esDivisible :: Int -> Int -> Bool
esDivisible numero divisor = mod numero divisor == 0

cantidadDeEmpleados :: String -> Int
cantidadDeEmpleados "Acme" = 10
cantidadDeEmpleados empresa | last empresa < head empresa = letrasIntermedias empresa
                            | esCapicua empresa && even cantLetras = letrasIntermedias empresa * 2
                            | esDivisible cantLetras 3 || esDivisible cantLetras 7 = 3
                            | otherwise = 0
                            where
                              cantLetras = length empresa