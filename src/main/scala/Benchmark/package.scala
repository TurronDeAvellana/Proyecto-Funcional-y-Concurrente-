import Datos._
import Itinerarios._
import ItinerariosPar._
import org.scalameter._
package object Benchmark {
  type AlgoritmoItinerarios = (List[Vuelo], List[Aeropuerto]) => (String, String) => List[Itinerario]
  type AlgoritmoItinerarioSalida = (List[Vuelo], List[Aeropuerto]) => (String, String, Int, Int) => List[Itinerario]

  /*La funcion es llamada de la siguiente manera:
    Para los primeros 4 puntos se envian las dos funciones secuencial y paralela
    compararAlgoritmos(itinerarios,itinerariosPar)(vuelosCurso,aeropuertosCurso)("CLO","SMR")
    Para el punto 5 y su version paralela:
    compararAlgoritmoSalida(itinerarioSalida,itinerarioSalidaPar)(vuelosCurso,aeropuertosCurso)("CLO","SMR",12,20)    
  */
  
  def compararAlgoritmos(a1:AlgoritmoItinerarios, a2:AlgoritmoItinerarios)
                        (vuelos:List[Vuelo], aeropuertos:List[Aeropuerto])
                        (cod1: String, cod2: String):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(vuelos,aeropuertos)(cod1,cod2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(vuelos,aeropuertos)(cod1,cod2))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

  def compararAlgoritmosSalida(a1:AlgoritmoItinerarioSalida, a2:AlgoritmoItinerarioSalida)
                              (vuelos:List[Vuelo], aeropuertos:List[Aeropuerto])
                              (cod1: String, cod2: String, h:Int, m:Int ):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(vuelos,aeropuertos)(cod1,cod2,h,m))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(vuelos,aeropuertos)(cod1,cod2,h,m))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

}
