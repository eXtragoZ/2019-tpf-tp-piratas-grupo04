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

cantidadTesorosPirata :: Pirata -> Int
cantidadTesorosPirata pirata = length (botin pirata)

valoresTesorosPirata :: Pirata -> [Double]
valoresTesorosPirata pirata = map valor (botin pirata)

valorTotalTesoros :: Pirata -> Double
valorTotalTesoros = sum.valoresTesorosPirata --pirata = sum (valoresTesorosPirata pirata)

pirataEsAfortunado :: Pirata -> Bool
pirataEsAfortunado = (>10000).valorTotalTesoros