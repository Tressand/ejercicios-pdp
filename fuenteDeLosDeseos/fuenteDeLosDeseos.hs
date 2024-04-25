data Persona = NuevaPersona {
  edad :: Int,
  nombre :: String,
  felicidonios :: Int,
  sueños :: [Sueño]
}

type Sueño = String

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona | felicidonios persona > 100 = felicidonios persona * edad persona
                                  | felicidonios persona > 50 = length (sueños persona) * felicidonios persona
                                  | otherwise = 40

nombreLargo :: Persona -> Bool
nombreLargo = (>10) . length . nombre

personaSuertuda :: Persona -> Bool
personaSuertuda = even . (*3) . coeficienteDeSatisfaccion

nombreLindo :: Persona -> Bool
nombreLindo = (=='L') . head . nombre

type Transformacion = (Persona -> Persona)

cumplirSueños :: Transformacion
cumplirSueños persona = quitarSueños (ganarFelicidonios (aumentoPorSueñosCumplidos persona) persona)

aumentoPorSueñosCumplidos :: Persona -> Int
aumentoPorSueñosCumplidos persona = coeficienteDeSatisfaccion persona * length (sueños persona)

quitarSueños :: Transformacion
quitarSueños persona = persona {sueños = []}

ganarFelicidonios :: Int -> Transformacion
ganarFelicidonios aumento persona = persona {felicidonios = felicidonios persona + aumento}

fuenteDeLosDeseos :: Transformacion
fuenteDeLosDeseos = premiarSuertudo . ganarFelicidonios 10

premiarSuertudo :: Transformacion
premiarSuertudo persona | personaSuertuda persona = cumplirSueños persona
                        | otherwise = persona