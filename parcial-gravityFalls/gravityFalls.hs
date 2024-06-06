import Data.List
import Distribution.Compat.CharParsing (CharParsing(string))

type Condicion = Persona -> Bool

data Rareza = UnaRareza {
  peligrosidad :: Int,
  condiciones :: [Condicion]
}

type Objeto = String

data Persona = UnaPersona {
  edad :: Int,
  objetos :: [Objeto],
  experiencia :: Int
}

agregarExperiencia :: Int -> Persona -> Persona
agregarExperiencia valor persona = persona {experiencia = experiencia persona + valor}

tieneObjeto :: Objeto -> Persona -> Bool
tieneObjeto objeto persona = objeto `elem` objetos persona

tiene :: (Persona -> a) -> (a -> Bool) -> Persona -> Bool
tiene propiedad condicion = condicion . propiedad

siempreDetras :: Rareza
siempreDetras = UnaRareza 0 [const False]

peligrosidadDeGnomos :: Int -> Int
peligrosidadDeGnomos 1 = 0
peligrosidadDeGnomos cantidad = 2 ^ cantidad

grupoDeGnomos :: Int -> Rareza
grupoDeGnomos cantidad = UnaRareza (peligrosidadDeGnomos cantidad) [tieneObjeto "Soplador de hojas"]

fantasma :: Int -> [Condicion] -> Rareza
fantasma categoria = UnaRareza (categoria * 20)

puedeGanar :: Rareza -> Persona -> Bool
puedeGanar criatura persona = all ($ persona) (condiciones criatura)

enfrentarCriatura :: Rareza -> Persona -> Persona
enfrentarCriatura criatura persona  | puedeGanar criatura persona = agregarExperiencia (peligrosidad criatura) persona
                                    | otherwise = agregarExperiencia 1 persona

enfrentarGrupoCriaturas :: [Rareza] -> Persona -> Persona
enfrentarGrupoCriaturas grupo persona = foldr enfrentarCriatura persona grupo

experienciaTotalCombate :: [Rareza] -> Persona -> Int
experienciaTotalCombate grupo persona = experiencia (enfrentarGrupoCriaturas grupo persona)

personaEjemplo :: Persona
personaEjemplo = UnaPersona 13 ["Soplador de hojas"] 1

grupoEjemplo :: [Rareza]
grupoEjemplo = [siempreDetras,
                grupoDeGnomos 10,
                fantasma 3 [tiene edad (<13), tieneObjeto "Disfraz de Oveja"],
                fantasma 1 [tiene experiencia (>10)]]

combateEjemplo :: Int
combateEjemplo = experienciaTotalCombate grupoEjemplo personaEjemplo


zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = []
zipWithIf transformacion condicion (headA:tailA) (headB:tailB) | condicion headB = transformacion headA headB : zipWithIf transformacion condicion tailA tailB
                                                               | otherwise = headB : zipWithIf transformacion condicion (headA:tailA) tailB

abecedario :: [Char]
abecedario = ['a'..'z']

numeroLetra :: Char -> Int
numeroLetra = subtract (fromEnum 'a') . fromEnum

esLetra :: Char -> Bool
esLetra = flip elem abecedario

sumarValorALetra :: Int -> Char -> Char
sumarValorALetra valor = toEnum . (+ valor) . fromEnum

desplazarLetra :: Int -> Char -> Char
desplazarLetra valor letra | sumarValorALetra valor letra <= 'z' = sumarValorALetra valor letra
                           | otherwise = sumarValorALetra (valor - length abecedario) letra

desplazarLetras :: Int -> [Char] -> [Char]
desplazarLetras valor = map (desplazarLetra valor)

abecedarioDesde :: Char -> [Char]
abecedarioDesde caracter = desplazarLetras (numeroLetra caracter - 1) abecedario

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra base = desplazarLetra (length abecedario + 1 - numeroLetra base)

desencriptarConMascara :: [Char] -> String -> String
desencriptarConMascara = zipWithIf desencriptarLetra esLetra

cesar :: Char -> String -> String
cesar base = desencriptarConMascara (repeat base)

renderStringList :: [String] -> String
renderStringList = foldl1 (\ string1 string2 -> string1 ++ "  |  " ++ string2)

todasLasDesencripciones :: String -> String
todasLasDesencripciones texto = renderStringList (map (`cesar` texto) abecedario)

vignere :: String -> String -> String
vignere base = desencriptarConMascara (cycle base)