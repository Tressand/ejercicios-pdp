import Data.List
import Distribution.Simple.Program.HcPkg (list)

data Personaje = UnPersonaje {
  nombre :: String,
  puntaje :: Int,
  inventario :: [Material]
} deriving Show

type Material = String

steve :: Personaje
steve = UnPersonaje "Steve" 1000 ["Sueter", "Fogata", "Pollo", "Pollo"]

alex :: Personaje
alex = UnPersonaje "Alex" 1000 ["Madera", "Fosforo", "Pollo"]

--            input      , time , output
type Receta = ([Material], Int, Material)

rFogata :: Receta
rFogata = (["Madera", "Fosforo"], 10, "Fogata")

rPolloAsado :: Receta
rPolloAsado = (["Fogata", "Pollo"], 300, "Pollo Asado")

rSueter :: Receta
rSueter = (["Lana", "Agujas", "Tintura"], 600, "Sueter")

listaRecetas :: [Receta]
listaRecetas = [rFogata, rPolloAsado]

tieneObjetos :: [Material] -> Personaje -> Bool
tieneObjetos objetos pj = intersect objetos (inventario pj) == objetos

agregarObjetos :: [Material] -> Personaje -> Personaje
agregarObjetos objetos pj = pj {inventario = inventario pj ++ objetos}

quitarObjetos :: [Material] -> Personaje -> Personaje
quitarObjetos objetos pj = pj {inventario = inventario pj \\ objetos}

agregarPuntos :: Int -> Personaje -> Personaje
agregarPuntos puntos pj = pj {puntaje = puntaje pj + puntos}

-- CRAFT

craftear :: Receta -> Personaje -> Personaje
craftear (ingredientes, tiempo, resultado) pj | tieneObjetos ingredientes pj = (agregarPuntos (tiempo * 10). agregarObjetos [resultado] . quitarObjetos ingredientes) pj
                                              | otherwise = agregarPuntos (-100) pj

crafteosValiosos :: [Receta] -> Personaje -> [Receta]
crafteosValiosos listaRecetas pj = filter ((>= puntaje pj) . puntaje . flip craftear pj) listaRecetas

crafteoSucesivo :: [Receta] -> Personaje -> Personaje
crafteoSucesivo listaRecetas pj = foldl (flip craftear) pj listaRecetas

ordenPerfecto :: [Receta] -> Personaje -> Bool
ordenPerfecto listaRecetas pj = puntaje (crafteoSucesivo listaRecetas pj) >= puntaje (crafteoSucesivo (reverse listaRecetas) pj)


-- MINE
--          requirement, yieldings
type Bioma = (Material , [Material])

type Herramienta = ([Material] -> Material)

pico :: Int -> Herramienta
pico precision = (!! precision)

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

calador :: Herramienta
calador lista =  lista !! (flip div 2 . length) lista

taladro :: Herramienta
taladro lista | any (\ material -> material == "Piedra") lista = "Diamante"

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta pj (requisito, materiales) | tieneObjetos [requisito] pj = agregarObjetos [herramienta materiales] pj
                                             | otherwise = pj

