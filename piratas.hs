import Text.Show.Functions

data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Double
} deriving (Show)

-- La cantidad de tesoros de un pirata

cantidadTesorosPirata :: Pirata -> Int
cantidadTesorosPirata = length.botin
-- length.botin

valoresTesorosPirata :: Pirata -> [Double]
valoresTesorosPirata pirata = map valor (botin pirata)

valorTotalTesoros :: Pirata -> Double
valorTotalTesoros = sum.valoresTesorosPirata --pirata = sum (valoresTesorosPirata pirata)

-- Si un pirata es afortunado, lo cual es cierto si el valor total de su botín supera los 10000

pirataEsAfortunado :: Pirata -> Bool
pirataEsAfortunado = (>10000).valorTotalTesoros

-- Si dos piratas tienen un mismo tesoro, pero de valor diferente

mismoTesoroDistintoValor :: Tesoro -> Tesoro -> Bool
mismoTesoroDistintoValor tesoro1 tesoro2 = (nombreTesoro tesoro1 == nombreTesoro tesoro2) && (valor tesoro1 /= valor tesoro2)

mismoTesoroDistintoValorEnBotin :: [Tesoro] -> Tesoro -> Bool
mismoTesoroDistintoValorEnBotin tesoros tesoro = any (mismoTesoroDistintoValor tesoro) tesoros

mismosTesorosDistintoValorEnOtroBotin :: [Tesoro] -> [Tesoro] -> Bool
mismosTesorosDistintoValorEnOtroBotin tesoros1 tesoros2 = any (mismoTesoroDistintoValorEnBotin tesoros1) tesoros2

pirataConMismoTesoroDistintoValorQueOtroPirata :: Pirata -> Pirata -> Bool
pirataConMismoTesoroDistintoValorQueOtroPirata pirata1 pirata2 = mismosTesorosDistintoValorEnOtroBotin (botin pirata1) (botin pirata2)

--esta de mas
piratasConMismoTesoroDistintoValor :: [Pirata] -> Bool
piratasConMismoTesoroDistintoValor [] = False
piratasConMismoTesoroDistintoValor (pirata:piratas)
    | any (pirataConMismoTesoroDistintoValorQueOtroPirata pirata) piratas = True
    | otherwise = piratasConMismoTesoroDistintoValor piratas

-- El valor del tesoro más valioso de un pirata.

tesoroMasValiosoDeUnPirata :: Pirata -> Double
tesoroMasValiosoDeUnPirata pirata = maximum (valoresTesorosPirata pirata)

-- Como queda el pirata luego de adquirir un nuevo tesoro
pirataAdquiereNuevoTesoro :: Tesoro ->  Pirata -> Pirata
pirataAdquiereNuevoTesoro tesoro pirata = pirata {botin = tesoro:(botin pirata)}

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

--saquearOro :: Tesoro -> Bool
--saquearOro = tesoroConNombre "oro" 

--saquearValiosoYSombrero :: Tesoro -> Bool
--saquearValiosoYSombrero = tesoroEsSaqueable [tesoroEsValioso, tesoroConNombre "sombrero"]

type FormaDeSaqueo = Tesoro -> Bool 

saquear :: Pirata -> (Tesoro -> Bool) -> Tesoro -> Pirata
saquear pirata formaDeSaquear tesoro
        | formaDeSaquear tesoro = pirataAdquiereNuevoTesoro tesoro pirata
        | otherwise = pirata

-- Navegando los siete mares

data Barco = Barco {
    nombreBarco :: String,
    tripulacion :: [Pirata],
    formaDeSaquear :: Tesoro -> Bool
} deriving (Show)

--Un pirata se incorpora a la tripulación de un barco
barcoIncorporaTripulante :: Barco -> Pirata -> Barco
barcoIncorporaTripulante barco pirata = barco {tripulacion = pirata:(tripulacion barco)}

--Un pirata abandona la tripulación de un barco
barcoAbandonaTripulante :: Barco -> Pirata -> Barco
barcoAbandonaTripulante barco pirata = barco {tripulacion = filter(\tipulante -> nombrePirata tipulante /= nombrePirata pirata)(tripulacion barco)}
-- delegar en otra funcion pirataConNombre

--Un barco ancla en Isla Deshabitada
data Isla = Isla {
    nombreIsla :: String,
    elementoTipico :: Tesoro
} deriving (Show)

anclarEnIslaDeshabitada :: Barco -> Isla -> Barco
anclarEnIslaDeshabitada barco isla = barco { tripulacion = map (pirataAdquiereNuevoTesoro (elementoTipico isla)) (tripulacion barco) }

--Un barco saquea una ciudad
data Ciudad = Ciudad {
    nombre :: String,
    tesoros :: [Tesoro]
} deriving (Show)

_saquear :: (Tesoro -> Bool) -> Pirata -> Tesoro -> Pirata
_saquear formaDeSaquear pirata tesoro = saquear pirata formaDeSaquear tesoro

piratasSaqueanTesoros :: (Tesoro -> Bool) -> [Pirata] -> [Tesoro] -> [Pirata]
piratasSaqueanTesoros formaDeSaquear piratas tesoros = zipWith (_saquear formaDeSaquear) piratas tesoros

barcoSaqueaCiudad :: Barco -> Ciudad -> Barco
barcoSaqueaCiudad barco ciudad = barco { tripulacion = piratasSaqueanTesoros (formaDeSaquear barco) (tripulacion barco) (tesoros ciudad) }

--Un barco aborda otro en altamar:
-- Cuando un barco aborda a otro que se encuentra en altamar, los piratas atacan uno a uno a los del barco abordado,
-- robando sus tesoros valiosos, y vuelven a su barco

tesorosValiosos :: [Tesoro] -> [Tesoro]
tesorosValiosos = filter (tesoroEsValioso)

pirataAdquiereNuevosTesoros :: Pirata -> [Tesoro] -> Pirata
pirataAdquiereNuevosTesoros pirata tesoros = pirata { botin = tesoros ++ (botin pirata)}

robarTesorosValiosos :: Pirata -> Pirata -> Pirata
robarTesorosValiosos pirata pirataRobado = pirataAdquiereNuevosTesoros pirata (tesorosValiosos (botin pirataRobado))

piratasRobanTesorosValiosos :: [Pirata] -> [Pirata] -> [Pirata]
piratasRobanTesorosValiosos piratas piratasRobados = zipWith robarTesorosValiosos piratas piratasRobados ++ drop (length piratasRobados) piratas

piratasPierdenTesorosValiosos :: [Pirata] -> [Pirata]
piratasPierdenTesorosValiosos piratas = map pirataPierdeTesorosValiosos piratas

barcoAbordaOtroBarco :: Barco -> Barco -> Barco
barcoAbordaOtroBarco barco barcoAbordado = barco { tripulacion = piratasRobanTesorosValiosos (tripulacion barco) (tripulacion barcoAbordado)}

barcoAbordadoPorOtro :: Barco -> Barco -> Barco
barcoAbordadoPorOtro barcoAbordado barco = barcoAbordado { tripulacion = piratasPierdenTesorosValiosos (tripulacion barcoAbordado) }

abordamientoDeBarcoEnAltaMar :: Barco -> Barco -> (Barco,Barco)
abordamientoDeBarcoEnAltaMar barco barcoAbordado = (barcoAbordaOtroBarco barco barcoAbordado,barcoAbordadoPorOtro barcoAbordado barco)

----- Datos para peliculas:

frascoArena = Tesoro "frasco de arena" 0
brujula = Tesoro "Brujula que apunta a lo que mas deseas" 10000
cajitaMusical = Tesoro "Cajita musical" 1
doblones = Tesoro "Doblones" 100
frascoArena2 = Tesoro "frasco de arena" 1
oro = Tesoro "oro" 100
sombrero = Tesoro "sombrero" 20
moneda = Tesoro "moneda del cofre muerto" 100
espada = Tesoro "espada de hierro" 50
cuchillo = Tesoro "cuchillo del padre" 5
botellaRon = Tesoro "Botella de Ron" 25
joyas = Tesoro "Set de joyas" 175
joyas2 = Tesoro "Set de joyas" 150
bolsonMoneda = Tesoro "Bolson de monedas" 110
bolsonMoneda2 = Tesoro "Bolson de monedas" 120

jackSparrow = Pirata "Jack Sparrow" [frascoArena, brujula]
anneBonny = Pirata "Anne Bonny" [doblones, frascoArena2]
elizabethSwann = Pirata "Elizabeth Swann" [moneda, espada]
willTurner = Pirata "Will Turner" [cuchillo]

perlaNegra = Barco "Perla Negra" [jackSparrow, anneBonny, elizabethSwann, willTurner] saquearValiosoYSombrero

davidJones = Pirata "David Jones" [cajitaMusical, oro, sombrero]
maccus = Pirata "Maccus" [frascoArena]
clacker = Pirata "Clacker" [oro]
jimmyLegs = Pirata "Jimmy Legs" [botellaRon]
koleniko = Pirata "Koleniko" [espada]
palifico = Pirata "Palifico" [frascoArena]

holandesErrante = Barco "Holandes Errante" [davidJones, maccus, clacker, jimmyLegs, koleniko, palifico] saquearOro 

islaTortuga = Isla "Isla Tortuga" frascoArena2
islaDelRon = Isla "Isla del Ron" botellaRon

portRoyal = Ciudad "Port Royal" [joyas, bolsonMoneda, joyas2, espada, bolsonMoneda2, joyas, joyas2]
carmenPatagones = Ciudad "Carmen de Patagones" [espada, oro, oro, oro]

------Pelicula ejemplo:

--La tripulación del Perla Negra desembarca en la IslaDelRon y todos se llevan una botella.
ejemploEscena1 = anclarEnIslaDeshabitada perlaNegra islaDelRon
--El Perla Negra ataca Port Royal, donde hay muchos tesoros.
ejemploEscena2 = barcoSaqueaCiudad ejemploEscena1 portRoyal
--El Holandes Errante pasa por la Isla Tortuga y luego hace un largo viaje para atacar Carmen de Patagones, donde hay pocos tesoros.
ejemploEscena3 = anclarEnIslaDeshabitada holandesErrante islaTortuga
ejemploEscena4 = barcoSaqueaCiudad ejemploEscena3 carmenPatagones
--El Perla Negra aborda al Holandes Errante
ejemploEscena5 = barcoAbordaOtroBarco ejemploEscena2 ejemploEscena4

------Pelicula

-- jackSparrow se une al perla negra
escena1 = barcoIncorporaTripulante perlaNegra jackSparrow

-- el perla negra desembarca en una isla desierta
escena2 = anclarEnIslaDeshabitada escena1 islaTortuga

-- elholandes errante ve el perla negra anclado y lo ataca
escena3 = barcoAbordadoPorOtro escena2 holandesErrante

-- el perla negra ataca portRoyal para recuperar tesoros
escena4 = barcoSaqueaCiudad escena3 portRoyal

-- Jack Sparrow y el perla negra se vengan del holandes
escena5 = barcoAbordaOtroBarco escena4 holandesErrante
