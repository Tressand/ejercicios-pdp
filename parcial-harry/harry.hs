data Postre = UnPostre {
  nombre :: String,
  sabores :: [String],
  peso :: Float,
  temperatura :: Float
} deriving Eq

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabores postre ++ [sabor]}

vaciarSabores :: Postre -> Postre
vaciarSabores postre = postre {sabores = []}

sumarAPesoPorcentual :: Float -> Postre -> Postre
sumarAPesoPorcentual porcentaje postre = postre {peso = peso postre * (1 + porcentaje / 100)}

sumarATemperatura :: Float -> Postre -> Postre
sumarATemperatura valor postre = postre {temperatura = temperatura postre + valor}

bizcochoBorracho :: Postre
bizcochoBorracho = UnPostre "Bizcocho Borracho" ["Fruta", "Crema"] 100 25

tarta :: Postre
tarta = UnPostre "Tarta" ["Melaza"] 50 0


type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = sumarAPesoPorcentual (-5) . sumarATemperatura 1

immobulus :: Hechizo
immobulus postre = sumarATemperatura (-(temperatura postre)) postre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = sumarAPesoPorcentual (-10) . agregarSabor "Concentrado"

diffindo :: Float -> Hechizo
diffindo valor = sumarAPesoPorcentual (-valor)

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra = immobulus . vaciarSabores


estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 && not (null (sabores postre)) && temperatura postre > 0

mesaLista :: Hechizo -> [Postre] -> Bool
mesaLista hechizo = all (estaListo . hechizo)

obtenerListos :: [Postre] -> [Postre]
obtenerListos = filter estaListo

pesoPromedioMesaLista :: [Postre] -> Float
pesoPromedioMesaLista mesa = sum (map peso (obtenerListos mesa)) / (fromIntegral (length (obtenerListos mesa)) :: Float)


data Mago = UnMago {
  hechizos :: [Hechizo],
  horrocruxes :: Int
}

agregarHorrocruxes :: Int -> Mago -> Mago
agregarHorrocruxes cantidad mago = mago {horrocruxes = horrocruxes mago + cantidad}

practicarHechizo :: Hechizo -> Mago -> Mago
practicarHechizo hechizo mago = mago {hechizos = hechizos mago ++ [hechizo]}

claseDeDefensa :: Hechizo -> Postre -> Mago -> Mago
claseDeDefensa hechizo postre mago | hechizo postre == avadaKedavra postre = (agregarHorrocruxes 1 . practicarHechizo hechizo) mago
                                   | otherwise = practicarHechizo hechizo mago

saboresDelResultado :: Postre -> Hechizo -> Int
saboresDelResultado postre hechizo = length (sabores (hechizo postre))

hechizoSuperior :: Postre -> Hechizo -> Hechizo -> Hechizo
hechizoSuperior postre hechizo1 hechizo2 | saboresDelResultado postre hechizo1 > saboresDelResultado postre hechizo2 = hechizo1
                                      | otherwise = hechizo2

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (hechizoSuperior postre) (hechizos mago)


mesaInfinita :: [Postre]
mesaInfinita = cycle [bizcochoBorracho, tarta]

magoOmnisciente :: Mago
magoOmnisciente = UnMago (cycle [incendio, immobulus, wingardiumLeviosa, avadaKedavra]) 999999

{-
  3A) Es imposible encontrar un hechizo que deje lista a una mesa infinita puesto que resulta
      necesario revisar todos los postres y eso por definición nunca puede ocurrir.

  3B) Es imposible encontrar un mejor hechizo para un mago omnisciente porque para eso habría
      que revisar todos los hechizos del mismo y siempre se pueden seguir obteniendo hechizos
      para comparar.
-}