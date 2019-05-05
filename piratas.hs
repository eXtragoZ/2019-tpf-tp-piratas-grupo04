import Text.Show.Functions

data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

-- Tesoros

data Tesoro = 
  UnTesoro {
      nombreTesoro :: String,
      valor :: Double }|
  BonoDefault {
    cotizaciones :: [Double] }|
  LeLiq {
    importeNominal :: Double,
    pais :: Pais  } deriving Show

class InstanciaTesoro a where
  nombreDeTesoro :: a -> String
  valorTesoro :: a -> Double

instance InstanciaTesoro Tesoro where
  nombreDeTesoro (UnTesoro nombre _) = nombre
  nombreDeTesoro (BonoDefault _) = "Bono"
  nombreDeTesoro (LeLiq _ pais) = "Leliq" ++ " " ++ (nombrePais pais)
  valorTesoro (UnTesoro _ valor) = valor
  valorTesoro (BonoDefault cotizaciones) = valorDeCotizaciones cotizaciones
  valorTesoro (LeLiq imp pais) = valorDeLeLiq imp pais

valorDeCotizaciones :: [Double] -> Double
valorDeCotizaciones cotizaciones = (maximum cotizaciones - minimum cotizaciones) * 1.5

valorDeLeLiq :: Double -> Pais -> Double
valorDeLeLiq imp pais = imp * (obtenerTasa (tasaPais pais))

obtenerTasa :: Double -> Double
obtenerTasa tasa = (tasa / 100) + 1

data Pais = Pais {
  nombrePais :: String,
  tasaPais :: Double } deriving (Show)
-- La cantidad de tesoros de un pirata

cantidadTesorosPirata :: Pirata -> Int
cantidadTesorosPirata = length.botin
-- length.botin

valoresTesorosPirata :: Pirata -> [Double]
valoresTesorosPirata pirata = map valorTesoro (botin pirata)

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

-- El valor del tesoro más valioso de un pirata.

tesoroMasValiosoDeUnPirata :: Pirata -> Double
tesoroMasValiosoDeUnPirata = maximum.valoresTesorosPirata

-- Como queda el pirata luego de adquirir un nuevo tesoro
pirataAdquiereNuevoTesoro :: Tesoro ->  Pirata -> Pirata
pirataAdquiereNuevoTesoro tesoro pirata = pirata {botin = tesoro:(botin pirata)}

-- Como queda el pirata luego de perder todos los tesoros valiosos, que son los que tienen un valor mayor a 100
--tesoroEsValioso :: Tesoro -> Bool
--tesoroEsValioso = (>=100).valorTesoro

tesorosNoValiosos :: [Tesoro] -> [Tesoro]
tesorosNoValiosos = filter (not.tesoroEsValioso)

pirataPierdeTesorosValiosos :: Pirata -> Pirata
pirataPierdeTesorosValiosos pirata = pirata {botin = tesorosNoValiosos (botin pirata)}

-- Como queda el pirata luego de perder todos los tesoros con un nombre dado
--tesoroConNombre :: String -> Tesoro -> Bool
--tesoroConNombre nombre tesoro = (nombreDeTesoro tesoro) == nombre

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
-- noSaqueaTesoro :: Tesoro -> Bool
-- noSaqueaTesoro tesoro = False

-- Existe una forma más compleja que consiste en una conjunción de las anteriores. Esto significa que se quedan con los tesoros que cumplan al menos una de entre un conjunto de maneras se saquear.

type FormaDeSaqueo = Tesoro -> Bool 

--tesoroEsSaqueable :: [FormaDeSaqueo] -> Tesoro -> Bool
--tesoroEsSaqueable formasDeSaquear tesoro = any($tesoro) formasDeSaquear

class Saqueador a where
  tesoroEsValioso :: a -> Bool
  tesoroConNombre :: String -> a -> Bool
  noSaqueaTesoro :: a -> Bool
  tesoroEsSaqueable :: [FormaDeSaqueo] -> a -> Bool
  saqueoBuitre :: a -> Bool
  saqueoFobico :: String -> a -> Bool
  saquear :: Pirata -> FormaDeSaqueo -> a -> Pirata

instance Saqueador Tesoro where
  tesoroEsValioso = (>=100).valorTesoro
  tesoroConNombre nombre tesoro = (nombreDeTesoro tesoro) == nombre
  noSaqueaTesoro _ = False
  tesoroEsSaqueable formasDeSaquear tesoro = any($tesoro) formasDeSaquear
  saqueoBuitre (BonoDefault _) = True
  saqueoBuitre _ = False
  saqueoFobico nombre = not.(tesoroConNombre nombre)
  saquear pirata formaDeSaquear tesoro
          | formaDeSaquear tesoro = pirataAdquiereNuevoTesoro tesoro pirata
          | otherwise = pirata

-- Navegando los siete mares

data Barco = Barco {
    nombreBarco :: String,
    tripulacion :: [Pirata],
    formaDeSaquear :: FormaDeSaqueo
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

_saquear :: FormaDeSaqueo -> Pirata -> Tesoro -> Pirata
_saquear formaDeSaquear pirata tesoro = saquear pirata formaDeSaquear tesoro

piratasSaqueanTesoros :: FormaDeSaqueo -> [Pirata] -> [Tesoro] -> [Pirata]
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

-- Universidad Pirata

data TipoUniversidad = AntiDictaminante | BuitreAlternativa | AtlanticaInofensiva

class UniversidadPirata a where
  desarrollarSaqueo :: a -> Barco -> Barco

instance UniversidadPirata TipoUniversidad where
  desarrollarSaqueo AntiDictaminante barco = barco --{  formaDeSaquear = (not formaDeSaquear)}
  desarrollarSaqueo BuitreAlternativa barco = barco { formaDeSaquear = (tesoroEsSaqueable ([(formaDeSaquear barco)] ++ [saqueoBuitre, tesoroEsValioso]))}
  desarrollarSaqueo AtlanticaInofensiva barco = barco

----- Datos para peliculas:

frascoArena = UnTesoro "frasco de arena" 0
brujula = UnTesoro "Brujula que apunta a lo que mas deseas" 10000
cajitaMusical = UnTesoro "Cajita musical" 1
doblones = UnTesoro "Doblones" 100
frascoArena2 = UnTesoro "frasco de arena" 1
oro = UnTesoro "oro" 100
sombrero = UnTesoro "sombrero" 20
moneda = UnTesoro "moneda del cofre muerto" 100
espada = UnTesoro "espada de hierro" 50
cuchillo = UnTesoro "cuchillo del padre" 5
botellaRon = UnTesoro "Botella de Ron" 25
joyas = UnTesoro "Set de joyas" 175
joyas2 = UnTesoro "Set de joyas" 150
bolsonMoneda = UnTesoro "Bolson de monedas" 110
bolsonMoneda2 = UnTesoro "Bolson de monedas" 120

bono = BonoDefault [100,20,36,42]

argentina = Pais "Argentina" 74.00

leliq = LeLiq 100 argentina

jackSparrow = Pirata "Jack Sparrow" [frascoArena, brujula]
anneBonny = Pirata "Anne Bonny" [doblones, frascoArena2]
elizabethSwann = Pirata "Elizabeth Swann" [moneda, espada]
willTurner = Pirata "Will Turner" [cuchillo]

perlaNegra = Barco "Perla Negra" [jackSparrow, anneBonny, elizabethSwann, willTurner] (tesoroEsSaqueable [tesoroEsValioso, tesoroConNombre "sombrero"])

davidJones = Pirata "David Jones" [cajitaMusical, oro, sombrero]
maccus = Pirata "Maccus" [frascoArena]
clacker = Pirata "Clacker" [oro]
jimmyLegs = Pirata "Jimmy Legs" [botellaRon]
koleniko = Pirata "Koleniko" [espada]
palifico = Pirata "Palifico" [frascoArena]

holandesErrante = Barco "Holandes Errante" [davidJones, maccus, clacker, jimmyLegs, koleniko, palifico] (tesoroConNombre "oro") 

islaTortuga = Isla "Isla Tortuga" frascoArena2
islaDelRon = Isla "Isla del Ron" botellaRon

portRoyal = Ciudad "Port Royal" [joyas, bolsonMoneda, joyas2, espada, bolsonMoneda2, joyas, joyas2]
carmenPatagones = Ciudad "Carmen de Patagones" [espada, oro, oro, oro]

------Pelicula

-- jackSparrow se une al perla negra
escena1 :: Barco -> Pirata -> Barco
escena1 barco pirata = barcoIncorporaTripulante barco pirata

-- el perla negra desembarca en una isla desierta
escena2 :: Barco -> Isla -> Pirata -> Barco
escena2 barco1 isla pirata = anclarEnIslaDeshabitada (escena1 barco1 pirata) isla

-- elholandes errante ve el perla negra anclado y lo ataca
escena3 :: Barco -> Barco -> Isla -> Pirata -> Barco
escena3 barco1 barco2 isla pirata = barcoAbordadoPorOtro (escena2 barco1 isla pirata) barco2

-- el perla negra ataca portRoyal para recuperar tesoros
escena4 :: Barco -> Barco -> Ciudad -> Isla -> Pirata -> Barco
escena4 barco1 barco2 ciudad isla pirata = barcoSaqueaCiudad (escena3 barco1 barco2 isla pirata) ciudad

-- Jack Sparrow y el perla negra se vengan del holandes
escena5 :: Barco -> Barco -> Ciudad -> Isla -> Pirata -> Barco
escena5 barco1 barco2 ciudad isla pirata = barcoAbordaOtroBarco (escena4 barco1 barco2 ciudad isla pirata) barco2

pelicula :: Barco -> Barco -> Ciudad -> Isla -> Pirata -> Barco
pelicula barco1 barco2 ciudad isla pirata = escena5 barco1 barco2 ciudad isla pirata

------- Historias de los barcos:

type Situacion = Barco -> Barco

situacionWillSubeAlBarco :: Situacion
situacionWillSubeAlBarco barco = barcoIncorporaTripulante barco willTurner

situacionBarcoAbordaAlHolandesErrante :: Situacion
situacionBarcoAbordaAlHolandesErrante barco = barcoAbordaOtroBarco barco holandesErrante

situacionBarcoAtacaPortRoyale :: Situacion
situacionBarcoAtacaPortRoyale barco = barcoSaqueaCiudad barco portRoyal

historiaDelBarco :: Barco -> [Situacion] -> Barco
historiaDelBarco = foldl (\a b -> b a)

