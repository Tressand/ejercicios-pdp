data Hechicero = NuevoHechicero {
  nombre :: String,
  antiguedad :: Int,
  clan :: String,
  grado :: Float
}

nobara :: Hechicero
nobara = NuevoHechicero "Nobara" 1 "Kugisaki" 3

satoru :: Hechicero
satoru = NuevoHechicero "Satoru" 15 "Gojo" 0

maki :: Hechicero
maki = NuevoHechicero "Maki" 3 "Zenin" 4

yugi :: Hechicero
yugi = NuevoHechicero "Yugi" 0 "Itadori" 1

experimientado :: Hechicero -> Bool
experimientado = (>1) . antiguedad

type Equipo = [Hechicero]

equipoPreparado :: Equipo -> Bool
equipoPreparado = (>3) . length

subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero = hechicero {grado = max 0 (grado hechicero - 1)}

prestigio :: Hechicero -> Bool
prestigio = esClanPrestigioso . clan

esClanPrestigioso :: String -> Bool
esClanPrestigioso "Zenin" = True;
esClanPrestigioso "Gojo" = True;
esClanPrestigioso "Kamo" = True;
esClanPrestigioso x = False;

invencible :: Equipo -> Bool
invencible = elem 0 . map grado

grupoFavorito :: Equipo -> Bool
grupoFavorito = all prestigio

expertos :: Equipo -> Equipo
expertos = filter experimientado

grupoCapaz :: Equipo -> Equipo
grupoCapaz grupo | esGrupoCapaz grupo = map subirDeGrado grupo
                 | otherwise = grupo

esGrupoCapaz :: Equipo -> Bool
esGrupoCapaz equipo = invencible equipo || equipoPreparado equipo

type Criterio = (Hechicero -> Hechicero -> Hechicero)

comparar :: String -> Criterio
comparar "tryhard" = criterioTryhard
comparar "burocratico" = criterioBurocratico
comparar "intimidante" = criterioIntimidante
comparar "sigilo" = criterioSigilo
comparar x = error "Criterio invalido"

criterioTryhard :: Criterio
criterioTryhard opcionA opcionB | nivelTryhard opcionA > nivelTryhard opcionB = opcionA
                                | nivelTryhard opcionA < nivelTryhard opcionB = opcionB
                                | otherwise = error "Su nivel es el mismo, toca tirar moneda"

nivelTryhard :: Hechicero -> Float
nivelTryhard hechicero = 1 / (grado hechicero + 1)

criterioBurocratico :: Criterio
criterioBurocratico opcionA opcionB | length (clan opcionA) > length (clan opcionB) = opcionA
                                    | length (clan opcionA) < length (clan opcionB) = opcionB
                                    | otherwise = error "Su nivel es el mismo, toca tirar moneda"

criterioIntimidante :: Criterio
criterioIntimidante opcionA opcionB | maximum (clan opcionA) > maximum (clan opcionB) = opcionA
                                    | maximum (clan opcionA) < maximum (clan opcionB) = opcionB
                                    | otherwise = error "Su nivel es el mismo, toca tirar moneda"

criterioSigilo :: Criterio
criterioSigilo opcionA opcionB | antiguedad opcionA * 6 > antiguedad opcionB * 6 = opcionA
                               | antiguedad opcionA * 6 < antiguedad opcionB * 6 = opcionB
                               | otherwise = error "Su nivel es el mismo, toca tirar moneda"