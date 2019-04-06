data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Double
} deriving (Show)

jackSparrow = Pirata "Jack Sparrow" [
    Tesoro "frasco de arena" 0, 
    Tesoro "Brujula que apunta a lo que mas deseas" 1000
    ]

davidJones = Pirata "David Jones" [
    Tesoro "Cajita musical" 1
    ]

anneBonny = Pirata "Anne Bonny" [
    Tesoro "Doblones" 100,
    Tesoro "frasco de arena" 1
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
pirataAdquiereNuevoTesoro pirata tesoro = Pirata (nombrePirata pirata) ((botin pirata) ++ [tesoro])

-- Como queda el pirata luego de perder todos los tesoros valiosos, que son los que tienen un valor mayor a 100
tesorosNoValiosos :: [Tesoro] -> [Tesoro]
tesorosNoValiosos [] = []
tesorosNoValiosos tesoros = filter (\x -> valor x < 100) tesoros

pirataPierdeTesorosValiosos :: Pirata -> Pirata
pirataPierdeTesorosValiosos pirata = Pirata (nombrePirata pirata) (tesorosNoValiosos (botin pirata))

-- Como queda el pirata luego de perder todos los tesoros con un nombre dado
tesoroNoCondiceConNombre :: [Tesoro] -> String -> [Tesoro]
tesoroNoCondiceConNombre [] x = []
tesoroNoCondiceConNombre tesoros x = filter (\y -> not (nombreTesoro y == x)) tesoros

pirataPierdeTesorosConNombre :: Pirata -> String -> Pirata
pirataPierdeTesorosConNombre pirata x = Pirata (nombrePirata pirata) (tesoroNoCondiceConNombre (botin pirata) x)
