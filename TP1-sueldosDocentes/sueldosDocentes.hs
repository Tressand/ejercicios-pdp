calcularSueldoBasico :: String -> Float
calcularSueldoBasico "titular" = 149000
calcularSueldoBasico "adjunto" = 116000
calcularSueldoBasico "ayudante" = 66000
calcularSueldoBasico x = error "404: Cargo not Found"

calcularMultiplicadorAntiguedad :: Int -> Float
calcularMultiplicadorAntiguedad antiguedad | antiguedad < 3 = 1     --(0;3) = 100%
                                           | antiguedad < 5 = 1.20  --[3;5) = 120%
                                           | antiguedad < 10 = 1.30 --[5;10) = 130%
                                           | antiguedad < 24 = 1.5  --[10;24) = 150%
                                           | antiguedad >= 24 = 2.2 --[24;inf) = 220%
                                           | otherwise = error "No te hagas el vivo, no se puede tener antiguedad negativa"

calcularMultiplicadorHoras :: Float -> Float
calcularMultiplicadorHoras horas | horas < 5 || horas > 50 = error "No existe persona que trabaje esa cantidad de horas por semana"
                                 | otherwise = fromInteger (round (horas / 10)) :: Float

sueldoNov2023 :: String -> Float -> Int -> Float
sueldoNov2023 cargo horas antiguedad = sueldoBasico * multiplicadorAntiguedad * multiplicadorHoras
                                        where
                                          sueldoBasico = calcularSueldoBasico cargo
                                          multiplicadorAntiguedad = calcularMultiplicadorAntiguedad antiguedad
                                          multiplicadorHoras = calcularMultiplicadorHoras horas

sueldoFeb2024 :: String -> Float -> Int -> Float
sueldoFeb2024 cargo horas antiguedad = sueldoNov2023 cargo horas antiguedad * 1.22

-- Nota: Según los datos de la canasta básica de la consigna: (aproximados proporcionalmente, ligeramente incorrectos)
-- Coste de un adulto = $126k
-- Coste de un menor = $69k
-- Coste de un adulto mayor = $58k

-- CBT = Canasta Básica Total
cbtNov2023 :: Int -> Float
cbtNov2023 1 = 126000
cbtNov2023 2 = 195000 -- Dato inventado asumiendo 1 adulto y 1 menor
cbtNov2023 3 = 310000
cbtNov2023 4 = 390000
cbtNov2023 5 = 410000
cbtNov2023 x = error "La CBT estipulada no incluye un valor para esta cantidad de integrantes"

-- CBT = Canasta Básica Total
cbtFeb2024 :: Int -> Float
cbtFeb2024 integrantes = cbtNov2023 integrantes * 1.71

-- Docente de ejemplo: "adjunto" 30 1 3 
deltaPobreza :: String -> Float -> Int -> Int -> String -> Float
deltaPobreza cargo horas antiguedad integrantes fecha | fecha == "Nov2023" = sueldoNov2023 cargo horas antiguedad - cbtNov2023 integrantes
                                                      | fecha == "Feb2024" = sueldoFeb2024 cargo horas antiguedad - cbtFeb2024 integrantes