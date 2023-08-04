module Library where
import PdePreludat
import Text.Read (Lexeme(Number))

doble :: Number -> Number
doble numero = numero + numero

type PoderDelGolpe = Number
type FortalezaDelObjetivo = Number
type Presion = Number
type HorasEntrenamiento = Number
type Objetivo = String

poderDelGolpe :: HorasEntrenamiento -> Number
poderDelGolpe horas = 15 * horas

fortalezaObjetivo :: Objetivo -> Number
fortalezaObjetivo objetivo = 2 * length objetivo

presionDelGolpe :: HorasEntrenamiento -> Objetivo -> Presion
presionDelGolpe horas objetivo = poderDelGolpe horas / fortalezaObjetivo objetivo

-- --------------------------------

gomuGomuElephantGatlig :: Objetivo -> Presion
gomuGomuElephantGatlig = presionDelGolpe 180
-- gomuGomuElephantGatlig objetivo = presionDelGolpe 180 objetivo

golpesNormalesConsecutivos :: Objetivo -> Presion
golpesNormalesConsecutivos = presionDelGolpe 240

-- ---------------------------------

esDificil :: Objetivo -> Bool
esDificil objetivo = gomuGomuElephantGatlig objetivo < 100

esAccesible :: Objetivo -> Bool
esAccesible = (between 200 400).golpesNormalesConsecutivos.focalizar

between :: Number -> Number -> Number -> Bool
between menor mayor nro = nro > menor && nro < mayor

focalizar :: Objetivo -> Objetivo
focalizar objetivo = take 7 objetivo