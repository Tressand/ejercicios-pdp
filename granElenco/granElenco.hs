data Persona = UnaPersona {
nombre :: String,
recibioOscar :: Bool,
actuaciones :: [Actuacion]
} deriving Show

data Actuacion = UnaActuacion {
        pelicula :: Pelicula,
        valoracion :: Int
} deriving Show

type Pelicula = String

ultima :: Persona -> Pelicula
ultima actor = pelicula (last (actuaciones actor))

valorarUltima :: Persona -> Int
valorarUltima actor = valoracion (last (actuaciones actor))

peliculasPremiadas :: [Pelicula]
peliculasPremiadas = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

experienciaMinima :: Int
experienciaMinima = 10

primeraFila :: Persona -> Bool
primeraFila actor =
      (valorarUltima actor >= 3 && length (actuaciones actor) > 1)
  ||  (recibioOscar actor || valoracion (head (actuaciones actor)) == 5)
  ||  (length (actuaciones actor) >= experienciaMinima)

laAcademiaSeRectifica :: Persona -> Persona
laAcademiaSeRectifica actor = actor{recibioOscar = False}

--las Segundas Partes Nunca Fueron Buenas
lSPNFB :: Persona -> Persona
lSPNFB actor | valorarUltima actor > 3 = actor {
  actuaciones = actuaciones actor ++ [UnaActuacion {
    pelicula = "Secuela de " ++ ultima actor,
    valoracion = max (valorarUltima actor - 2) 0
  }]
}

villanos :: Persona -> Bool
villanos actor = all (> 3) (map valoracion (actuaciones actor)) && length (actuaciones actor) > 1