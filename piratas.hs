import Text.Show.Functions

data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Double
} deriving (Show)

frascoArena = Tesoro "frasco de arena" 0
brujula = Tesoro "Brujula que apunta a lo que mas deseas" 10000
cajitaMusical = Tesoro "Cajita musical" 1
doblones = Tesoro "Doblones" 100
frascoArena2 = Tesoro "frasco de arena" 1
oro = Tesoro "oro" 100
sombrero = Tesoro "sombrero" 20

jackSparrow = Pirata "Jack Sparrow" [frascoArena, brujula]

davidJones = Pirata "David Jones" [cajitaMusical]

anneBonny = Pirata "Anne Bonny" [doblones, frascoArena2]

piratas = [jackSparrow, davidJones, anneBonny]

-- La cantidad de tesoros de un pirata

cantidadTesorosPirata :: Pirata -> Int
cantidadTesorosPirata pirata = length (botin pirata)

valoresTesorosPirata :: Pirata -> [Double]
valoresTesorosPirata pirata = map valor (botin pirata)

valorTotalTesoros :: Pirata -> Double
valorTotalTesoros = sum.valoresTesorosPirata --pirata = sum (valoresTesorosPirata pirata)

-- Si un pirata es afortunado, lo cual es cierto si el valor total de su botín supera los 10000

pirataEsAfortunado :: Pirata -> Bool
pirataEsAfortunado = (>10000).valorTotalTesoros

-- Si dos piratas tienen un mismo tesoro, pero de valor diferente

mismoTesoroDistintoValor :: Tesoro -> Tesoro -> Bool
mismoTesoroDistintoValor tesoro1 tesoro2 = (nombreTesoro tesoro1 == nombreTesoro tesoro2) && not (valor tesoro1 == valor tesoro2)

mismoTesoroDistintoValorEnBotin :: [Tesoro] -> Tesoro -> Bool
mismoTesoroDistintoValorEnBotin tesoros tesoro = any (mismoTesoroDistintoValor tesoro) tesoros

mismosTesorosDistintoValorEnOtroBotin :: [Tesoro] -> [Tesoro] -> Bool
mismosTesorosDistintoValorEnOtroBotin tesoros1 tesoros2 = any (mismoTesoroDistintoValorEnBotin tesoros1) tesoros2

pirataConMismoTesoroDistintoValorQueOtroPirata :: Pirata -> Pirata -> Bool
pirataConMismoTesoroDistintoValorQueOtroPirata pirata1 pirata2 = mismosTesorosDistintoValorEnOtroBotin (botin pirata1) (botin pirata2)

piratasConMismoTesoroDistintoValor :: [Pirata] -> Bool
piratasConMismoTesoroDistintoValor [] = False
piratasConMismoTesoroDistintoValor (pirata:piratas)
    | any (pirataConMismoTesoroDistintoValorQueOtroPirata pirata) piratas = True
    | otherwise = piratasConMismoTesoroDistintoValor piratas

-- El valor del tesoro más valioso de un pirata.

tesoroMasValiosoDeUnPirata :: Pirata -> Double
tesoroMasValiosoDeUnPirata pirata = maximum (valoresTesorosPirata pirata)

-- Como queda el pirata luego de adquirir un nuevo tesoro
pirataAdquiereNuevoTesoro :: Pirata -> Tesoro -> Pirata
pirataAdquiereNuevoTesoro pirata tesoro = pirata {botin = tesoro:(botin pirata)}

-- Como queda el pirata luego de perder todos los tesoros valiosos, que son los que tienen un valor mayor a 100
tesoroEsValioso :: Tesoro -> Bool
tesoroEsValioso tesoro = valor tesoro >= 100

tesorosNoValiosos :: [Tesoro] -> [Tesoro]
tesorosNoValiosos = filter (not.tesoroEsValioso)

pirataPierdeTesorosValiosos :: Pirata -> Pirata
pirataPierdeTesorosValiosos pirata = pirata {botin = tesorosNoValiosos (botin pirata)}

-- Como queda el pirata luego de perder todos los tesoros con un nombre dado
tesoroConNombre :: String -> Tesoro -> Bool
tesoroConNombre nombre tesoro = (nombreTesoro tesoro) == nombre

tesoroNoCondiceConNombre :: [Tesoro] -> String -> [Tesoro]
tesoroNoCondiceConNombre tesoros nombre = filter (not.(tesoroConNombre nombre)) tesoros

pirataPierdeTesorosConNombre :: Pirata -> String -> Pirata
pirataPierdeTesorosConNombre pirata nombre = pirata {botin = (tesoroNoCondiceConNombre (botin pirata) nombre)}


--Temporada de saqueos

-- Sólo los tesoros valiosos.
-- tesoroEsValioso :: Tesoro -> Bool

-- Tesoros con objetos específicos, es decir, sólo tesoros cuyo nombre sea una palabra clave.
-- tesoroConNombre :: String -> Tesoro -> Bool

-- Existen los piratas con corazón que no saquean nada.
noSaqueaTesoro :: Tesoro -> Bool
noSaqueaTesoro tesoro = False

-- Existe una forma más compleja que consiste en una conjunción de las anteriores. Esto significa que se quedan con los tesoros que cumplan al menos una de entre un conjunto de maneras se saquear.
tesoroEsSaqueable :: [(Tesoro -> Bool)] -> Tesoro -> Bool
tesoroEsSaqueable formasDeSaquear tesoro = any($tesoro) formasDeSaquear

saquearOro :: Tesoro -> Bool
saquearOro = tesoroConNombre "oro"

saquearValiosoYSombrero :: Tesoro -> Bool
saquearValiosoYSombrero = tesoroEsSaqueable [tesoroEsValioso, tesoroConNombre "sombrero"]

saquear :: Pirata -> (Tesoro -> Bool) -> Tesoro -> Pirata
saquear pirata formaDeSaquear tesoro
        | formaDeSaquear tesoro = pirataAdquiereNuevoTesoro pirata tesoro
        | otherwise = pirata

-- Navegando los siete mares

data Barco = Barco {
    nombreBarco :: String,
    tripulacion :: [Pirata],
    formaDeSaquear :: Tesoro -> Bool
} deriving (Show)

perlaNegra = Barco "Perla Negra" [jackSparrow, anneBonny] saquearValiosoYSombrero

moneda = Tesoro "moneda del cofre muerto" 100
espada = Tesoro "espada de hierro" 50
cuchillo = Tesoro "cuchillo del padre" 5

elizabethSwann = Pirata "Elizabeth Swann" [moneda, espada]
willTurner = Pirata "Will Turner" [cuchillo]

--Un pirata se incorpora a la tripulación de un barco
barcoIncorporaTripulante :: Barco -> Pirata -> Barco
barcoIncorporaTripulante barco pirata = barco {tripulacion = pirata:(tripulacion barco)}

--Un pirata abandona la tripulación de un barco
barcoAbandonaTripulante :: Barco -> Pirata -> Barco
barcoAbandonaTripulante barco pirata = barco {tripulacion = filter(\tipulante -> nombrePirata tipulante /= nombrePirata pirata)(tripulacion barco)}

data Isla = Isla {
    nombreIsla :: String,
    elementoTipico :: Tesoro
} deriving (Show)

botellaRon = Tesoro "Botella de Ron" 25

islaTortuga = Isla "Isla Tortuga" frascoArena2
islaDelRon = Isla "Isla del Ron" botellaRon

anclarEnIslaDeshabitada :: Barco -> Isla -> Barco
anclarEnIslaDeshabitada barco isla = barco { tripulacion = map (`pirataAdquiereNuevoTesoro` elementoTipico isla) (tripulacion barco) }

data Ciudad = Ciudad {
    nombre :: String,
    tesoros :: [Tesoro]
} deriving (Show)

marDelPlata = Ciudad "Mar del Plata" [botellaRon, moneda, doblones, oro, sombrero]

piratasSaqueanTesoros :: (Tesoro -> Bool) -> [Pirata] -> [Tesoro] -> [Pirata]
piratasSaqueanTesoros formaDeSaquear [] [] = []
piratasSaqueanTesoros formaDeSaquear [] (tesoro:restoTesoros) = []
piratasSaqueanTesoros formaDeSaquear (pirata:restoPiratas) [] = []
piratasSaqueanTesoros formaDeSaquear (pirata:restoPiratas) (tesoro:restoTesoros) 
        = saquear pirata formaDeSaquear tesoro : piratasSaqueanTesoros formaDeSaquear restoPiratas restoTesoros

barcoSaqueaCiudad :: Barco -> Ciudad -> Barco
barcoSaqueaCiudad barco ciudad = barco { tripulacion = piratasSaqueanTesoros (formaDeSaquear barco) (tripulacion barco) (tesoros ciudad) }
