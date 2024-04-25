data Hechicero = NuevoHechicero {
  nombre :: String,
  antiguedad :: Int,
  clan :: String,
  grado :: Int
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

comparar :: Ord a => (Hechicero -> a) -> Hechicero -> Hechicero -> Hechicero
comparar nivel opcionA opcionB | nivel opcionA > nivel opcionB = opcionA
                               | nivel opcionA < nivel opcionB = opcionB
                               | otherwise = error "Su nivel es el mismo, toca tirar moneda"

type Nivel = Hechicero -> Int

nivelTryhard :: Nivel
nivelTryhard hechicero = div 1 (grado hechicero + 1)

nivelBurocratico :: Nivel
nivelBurocratico = length . clan

nivelIntimidante :: Nivel
nivelIntimidante hechicero = obtenerASCII (maximum (nombre hechicero))

obtenerASCII :: Char -> Int
obtenerASCII = fromEnum

nivelSigilo :: Nivel
nivelSigilo = (*6) . antiguedad