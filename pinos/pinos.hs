-- Funcion Auxiliar
metroACentimetro :: Float -> Float
metroACentimetro metros = metros * 100

-- 1.
pesoPino :: Float -> Float
pesoPino alturaEnMetros | alturaEnMetros <= 3 = metroACentimetro alturaEnMetros * 3
                        | otherwise = pesoPino 3 + metroACentimetro (alturaEnMetros - 3) * 2
-- 2.
esPesoUtil :: Float -> Bool
esPesoUtil pesoEnKilos = pesoEnKilos > 400 && pesoEnKilos < 1000

-- 3.
sirvePino :: Float -> Bool
sirvePino alturaEnMetros= esPesoUtil (pesoPino alturaEnMetros)

-- 4.
costoTransporte :: Float -> Float
costoTransporte alturaEnMetros | not (sirvePino alturaEnMetros) = error "El pino no sirve"
                               | pesoDelPino <= 500 = 5000
                               -- El rango (500, 600) no está definido por el enunciado 
                               | 600 <= pesoDelPino && pesoDelPino < 800 = pesoDelPino * 10
                               | pesoDelPino >= 800 = pesoDelPino * 10 + alturaEnCentimetros
                               | otherwise = error "Coste del pino no especificado por la empresa de transporte"
                               where
                                pesoDelPino = pesoPino alturaEnMetros
                                alturaEnCentimetros = metroACentimetro alturaEnMetros