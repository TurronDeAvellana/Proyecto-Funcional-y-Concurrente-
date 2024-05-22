import Datos._
import Itinerarios._

val itsCurso = itinerarios(vuelosCurso, aeropuertosCurso)

val its1 = itsCurso("MID", "SVCS")
val its2 = itsCurso("CLO", "SVCS")

val its3 = itsCurso("CLO", "SVO")

val its4 = itsCurso("CLO", "MEX")

val its5 = itsCurso("CTG", "PTY")

val itsEscalasCurso = itinerariosEscalas(vuelosCurso,aeropuertosCurso)

val itsc1 = itsEscalasCurso("MID","SVCS")
val itsc2 = itsEscalasCurso("CLO","SVCS")
val itsc3 = itsEscalasCurso("CLO","SVO")
val itsc4 = itsEscalasCurso("CLO","MEX")
val itsc5 = itsEscalasCurso("CTG","PTY")




