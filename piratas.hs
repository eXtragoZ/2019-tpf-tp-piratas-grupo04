data Pirata = Pirata {
    nombrePirata :: String,
    botin :: [Tesoro]
} deriving (Show)

data Tesoro = Tesoro {
    nombreTesoro :: String,
    valor :: Double
} deriving (Show)

frascoArena = Tesoro "frasco de arena" 0

piratas = [
    Pirata "Jack Sparrow" [
        Tesoro "Brujula que apunta a lo que mas deseas" 1000,
        frascoArena
        ],
    Pirata "David Jones" [
        Tesoro "Cajita musical" 1
        ],
    Pirata "Anne Bonny" [
        Tesoro "Doblones" 100,
        frascoArena
        ]
    ]  

cantidadTesorosPirata :: Pirata -> Int
cantidadTesorosPirata pirata = length (botin pirata)

--pirataEsAfortunado :: Pirata -> Bool
--pirataEsAfortunado pirata = sum valor botin pirata