import scala.collection.parallel.immutable._
import common._
//Se usa para la funcion secuencial de itinerarios llamada en funcion ItSalida
import Itinerarios.itinerarios

package object ItinerariosPar {
  def minutosDesdeMedianoche(hora: Int, minutos: Int, gmt: Int): Int = (60 * hora + minutos - gmt * 0.6).toInt

  

  
  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    def gmtDeAeropuerto(codigo: String): Int = aeropuertos.find(_.Cod == codigo).map(_.GMT).getOrElse(0)

    (cod1: String, cod2: String, h: Int, m: Int) => {
      val gmtDestino = gmtDeAeropuerto(cod2)

      val (citaEnMinutos:Int,vuelosPosibles:List[Itinerario]) = parallel(
        minutosDesdeMedianoche(h, m, gmtDestino),
        itinerarios(vuelos,aeropuertos)(cod1,cod2))
      //Aqui se está usando la vesion secuencial de itinerarios, se debe revisar

      /*
      Para cada Itinerario de vuelos revisa si este llega antes o a la hora de la cita,
      Si este sirve lo devuelve igual,
      De lo contrario devuelve una lista vacia que se eliminará despues.
      */
      def filtro (vuelos: Itinerario): Itinerario = {
        val vuelo = vuelos.last
        val llegadaEnMinutos = minutosDesdeMedianoche(vuelo.HL, vuelo.ML, gmtDestino)
        if (vuelos.nonEmpty && llegadaEnMinutos <= citaEnMinutos) {vuelos} else {List()}
      }

      /*
      Para cada vuelo hace de manera paralela la funcion filtro, despues con un map de join
      se obtiene la lista como tal y por ultimo con un filter descarta las listas vacias de filter
      */

      val vuelosValidos = (for {
         itinerario <- vuelosPosibles
         t = task(filtro(itinerario))
      } yield t).map(resultado => resultado.join).filter(vuelos => vuelos.nonEmpty)

      vuelosValidos.maxByOption { listaVuelos =>
        listaVuelos.headOption.map(vuelo => vuelo.HS * 60 + vuelo.MS).getOrElse(0)
      }.getOrElse(List.empty)
    }
  }

}
