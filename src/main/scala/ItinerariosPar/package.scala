import common._
import Datos._

//Se usa para la funcion secuencial de itinerarios llamada en funcion ItSalida
import Itinerarios.itinerarios

package object ItinerariosPar {
  def minutosDesdeMedianoche(hora: Int, minutos: Int, gmt: Int): Int = (60 * hora + minutos - gmt * 0.6).toInt

  //Version paralela de itinerariosAire
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

  // Helper function to find flights originating from a given airport code
  def findFlightsFrom(airportCode: String): List[Vuelo] = {
    vuelos.filter(_.Org == airportCode)
  }

  // DFS function to find all itineraries from cod1 to cod2
  def dfs(current: String, destination: String, visited: Set[String], path: List[Vuelo]): List[Itinerario] = {
    if (current == destination) {
      // If the current airport is the destination, return the current path as a valid itinerary
      List(path)
    } else {
      // Continue searching for itineraries
      val flightsFromCurrent = findFlightsFrom(current)
      val tasks = flightsFromCurrent.flatMap { flight =>
        if (!visited.contains(flight.Dst)) {
          // If the destination of the flight has not been visited, continue the search in parallel
          List(task { dfs(flight.Dst, destination, visited + flight.Dst, path :+ flight) })
        } else {
          List()
        }
      }
      tasks.flatMap(_.join())
    }
  }

  (cod1: String, cod2: String) => {
    dfs(cod1, cod2, Set(cod1), List())
  }
}

def calcularTiempoVuelo(vuelo: Vuelo, aeropuertoMap: Map[String, Int]): Int = {
  val gmtOrg = aeropuertoMap.getOrElse(vuelo.Org, 0)
  val gmtDst = aeropuertoMap.getOrElse(vuelo.Dst, 0)

  val salida = vuelo.HS * 60 + vuelo.MS + gmtOrg * 60
  val llegada = vuelo.HL * 60 + vuelo.ML + gmtDst * 60

  val diferenciaHvGMT = llegada - salida
  if (diferenciaHvGMT < 0) diferenciaHvGMT + (24 * 60) else diferenciaHvGMT
}

def calcularTiempoEspera(vueloActual: Vuelo, vueloSiguiente: Vuelo, aeropuertoMap: Map[String, Int]): Int = {
  val gmtDst = aeropuertoMap.getOrElse(vueloActual.Dst, 0)

  val llegadaActual = vueloActual.HL * 60 + vueloActual.ML + gmtDst * 60
  val salidaSiguiente = vueloSiguiente.HS * 60 + vueloSiguiente.MS + gmtDst * 60

  Math.max(0, salidaSiguiente - llegadaActual)
}






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


  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Guarda los valores de los itinerarios
    val obtenerItinerarios = itinerarios(vuelos, aeropuertos)

    // Función auxiliar
    def tresDeMenosEscalas(codigo1: String, codigo2: String): List[Itinerario] = {
      // Obtener los itinerarios posibles en paralelo
      val todosItinerariosTask = task { obtenerItinerarios(codigo1, codigo2) }

      // Esperamos a que la tarea se complete
      val todosItinerarios = todosItinerariosTask.join()

      // Ordenar los itinerarios por el número de escalas y cambios de vuelo
      val tiemposTask = todosItinerarios.map { itinerario =>
        task {
          (itinerario, itinerario.map(_.Esc).sum + (itinerario.length - 1))
        }
      }

      // Esperamos a que todas las tareas se completen
      val tiempos = tiemposTask.map(_.join())

      // Ordenar los itinerarios según el número de escalas y cambios de vuelo
      val itinerariosOrdenados = tiempos.sortBy(_._2).take(3).map(_._1)

      itinerariosOrdenados
    }

    tresDeMenosEscalas
  }









}
