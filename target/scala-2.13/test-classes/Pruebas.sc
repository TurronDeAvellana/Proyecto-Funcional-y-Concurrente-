import Datos._
import Itinerarios._

val itsCurso = itinerarios(vuelosCurso, aeropuertosCurso)

val its1 = itsCurso("MID", "SVCS")
val its2 = itsCurso("CLO", "SVCS")

val its3 = itsCurso("CLO", "SVO")

val its4 = itsCurso("CLO", "MEX")

val its5 = itsCurso("CTG", "PTY")

