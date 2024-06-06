data Nave = UnaNave {
  nombre :: String,
  durabilidad :: Int,
  escudo :: Int,
  ataque :: Int,
  poder :: Nave -> Nave
}

repetirFuncion :: (a -> a) -> Int -> (a -> a)
repetirFuncion funcion 0 = funcion
repetirFuncion funcion veces = funcion . repetirFuncion funcion (veces - 1)

sumarADurabilidad :: Int -> Nave -> Nave
sumarADurabilidad valor nave = nave {durabilidad = durabilidad nave + valor}

sumarAAtaque :: Int -> Nave -> Nave
sumarAAtaque valor nave = nave {ataque = ataque nave + valor}

sumarAEscudo :: Int -> Nave -> Nave
sumarAEscudo valor nave = nave {escudo = escudo nave + valor}

activarPoder :: Nave -> Nave
activarPoder nave = poder nave nave

turbo :: Nave -> Nave
turbo = sumarAAtaque 25

reparacion :: Nave -> Nave
reparacion = sumarADurabilidad 50 . sumarAAtaque (-30)


tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 turbo

xWing :: Nave
xWing = UnaNave "XWing" 300 150 100 reparacion

naveDVader :: Nave
naveDVader = UnaNave "Nave de Darth Vader" 500 300 200 (sumarADurabilidad 45 . repetirFuncion turbo 3)

milleniumFalcon :: Nave
milleniumFalcon = UnaNave "Millenium Falcon" 1000 500 50 (sumarAEscudo 100 . reparacion)

hfss :: Nave
hfss = UnaNave "Hocotate Freight Steam Ship" 9999 9999 0 (sumarAAtaque 30 . reparacion)

type Flota = [Nave]

durabilidadTotal :: Flota -> Int
durabilidadTotal flota = sum (map durabilidad flota)

recibirDaño :: Nave -> Int -> Nave
recibirDaño receptor ataque | escudo receptor >= ataque = receptor {escudo = escudo receptor - ataque}
                            | otherwise = receptor {escudo = 0, durabilidad = durabilidad receptor - (escudo receptor - ataque)}

defenderAtaque :: Nave -> Nave -> Nave
defenderAtaque atacante defensor = recibirDaño (activarPoder atacante) (ataque (activarPoder defensor))

fueraDeCombate :: Nave -> Bool
fueraDeCombate = (<= 0) . durabilidad

type Estrategia = (Nave -> Bool)

atacarAlDebil :: Estrategia
atacarAlDebil = (< 200) . escudo

atacarAlPeligroso :: Int -> Estrategia
atacarAlPeligroso valorAtaque = (> valorAtaque). ataque

atacarAlDestructible :: Nave -> Estrategia
atacarAlDestructible atacante = fueraDeCombate . defenderAtaque atacante

atacarAlReputado :: Estrategia
atacarAlReputado = (> 15). length . nombre


atacarAFlota :: Nave -> Flota -> Flota
atacarAFlota atacante = map (defenderAtaque atacante)

realizarMision :: Nave -> Flota -> Estrategia -> Flota
realizarMision atacante flotaDefensora estrategia = atacarAFlota atacante (filter estrategia flotaDefensora)

resultadoDeMision :: Nave -> Flota -> Estrategia -> Int
resultadoDeMision nave flota estrategia = durabilidadTotal (realizarMision nave flota estrategia)

elegirMejorEstrategia :: Nave -> Flota -> Estrategia -> Estrategia -> Estrategia
elegirMejorEstrategia nave flota strat1 strat2 | resultadoDeMision nave flota strat1 > resultadoDeMision nave flota strat2 = strat1
                                               | otherwise = strat2

planificarYEjecutar :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
planificarYEjecutar nave flota strat1 strat2 = realizarMision nave flota (elegirMejorEstrategia nave flota strat1 strat2)