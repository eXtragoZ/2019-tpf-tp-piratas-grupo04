data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Double
} deriving (Show)

frascoArena = Tesoro "frasco de arena" 0

jackSparrow = Pirata "Jack Sparrow" [
    frascoArena, 
    Tesoro "Brujula que apunta a lo que mas deseas" 1000
    ]

davidJones = Pirata "David Jones" [
    Tesoro "Cajita musical" 1
    ]

anneBonny = Pirata "Anne Bonny" [
    Tesoro "Doblones" 100,
    frascoArena
    ]

piratas = [jackSparrow,davidJones,anneBonny]  

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

mismoTesoroDistintoValorEnOtroBotin :: Tesoro -> [Tesoro] -> Bool
mismoTesoroDistintoValorEnOtroBotin tesoro [] = False
mismoTesoroDistintoValorEnOtroBotin tesoro1 (tesoro2:tesoros2)
    | mismoTesoroDistintoValor tesoro1 tesoro2 = True
    | otherwise = mismoTesoroDistintoValorEnOtroBotin tesoro1 tesoros2

mismosTesorosEnOtroBotin :: [Tesoro] -> [Tesoro] -> Bool
mismosTesorosEnOtroBotin [] [] = False
mismosTesorosEnOtroBotin [] tesoros2 = False
mismosTesorosEnOtroBotin (tesoro1:tesoros1) [] = False
mismosTesorosEnOtroBotin (tesoro1:tesoros1) tesoros2
    | mismoTesoroDistintoValorEnOtroBotin tesoro1 tesoros2 = True
    | otherwise = mismosTesorosEnOtroBotin tesoros1 tesoros2

mismosTesorosEnAmbosBotines :: [Tesoro] -> [Tesoro] -> Bool
mismosTesorosEnAmbosBotines [] [] = False
mismosTesorosEnAmbosBotines [] tesoro2 = False
mismosTesorosEnAmbosBotines tesoros1 [] = False
mismosTesorosEnAmbosBotines tesoros1 tesoros2 = mismosTesorosEnOtroBotin tesoros1 tesoros2 || mismosTesorosEnOtroBotin tesoros2 tesoros1

pirataConMismoTesoroDistintoValorQueOtrosPiratas :: Pirata -> [Pirata] -> Bool
pirataConMismoTesoroDistintoValorQueOtrosPiratas pirata [] = False
pirataConMismoTesoroDistintoValorQueOtrosPiratas pirata1 (pirata2:piratas)
    | mismosTesorosEnAmbosBotines (botin pirata1) (botin pirata2) = True
    | otherwise = pirataConMismoTesoroDistintoValorQueOtrosPiratas pirata1 piratas

piratasConMismoTesoroDistintoValor :: [Pirata] -> Bool
piratasConMismoTesoroDistintoValor [] = False
piratasConMismoTesoroDistintoValor (pirata:piratas)
    | pirataConMismoTesoroDistintoValorQueOtrosPiratas pirata piratas = True
    | otherwise = piratasConMismoTesoroDistintoValor piratas

-- El valor del tesoro más valioso de un pirata.

tesoroMasValiosoDeUnPirata :: Pirata -> Double
tesoroMasValiosoDeUnPirata pirata = maximum (valoresTesorosPirata pirata)
