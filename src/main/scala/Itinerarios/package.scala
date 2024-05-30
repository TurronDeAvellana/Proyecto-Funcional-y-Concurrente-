
import Datos._

package object Itinerarios {

  def minutosDesdeMedianoche(hora: Int, minutos: Int, gmt: Int): Int = (60 * hora + minutos - gmt * 0.6).toInt //Realmente siempre es entero pero scala no sabe

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {


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
      flightsFromCurrent.flatMap { flight =>
        if (!visited.contains(flight.Dst)) {
          // If the destination of the flight has not been visited, continue the search
          dfs(flight.Dst, destination, visited + flight.Dst, path :+ flight)
        } else {
          // If the destination has already been visited, skip to avoid cycles
          List()
        }
      }
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



  def itinerariosAire(vuelos : List [ Vuelo ] , aeropuertos : List [ Aeropuerto] ) : ( String , String )=>List [Itinerario ]= {
    //Obtiene todos los itinerarios podibles
    val funcionItinerario = itinerarios(vuelos, aeropuertos)

    //Calcula el tiempo total de un solo vuelo
    def funTiempo(vuelo:Vuelo) = {
      val a1 = aeropuertos.filter(_.Cod == vuelo.Org)
      val a2 = aeropuertos.filter(_.Cod == vuelo.Dst)

      val gmtSalida = (a1.head).GMT/100
      val gmtLlegada = (a2.head).GMT/100

      val horaSalida = (vuelo.HS + (vuelo.MS/60)) - gmtSalida
      val horaLlegada = (vuelo.HL + (vuelo.ML/60)) - gmtLlegada
      val tiempoVuelo = horaLlegada - horaSalida

      if (tiempoVuelo < 0) 24 + tiempoVuelo else tiempoVuelo
    }

    //Calcula el tiempo total de vuelo de un itinerario
    def calcularTiempo (v:Itinerario) = {
      val cadaTiempo =  for {
        j <- v

      } yield funTiempo(j)

      cadaTiempo.sum
    }

    //Funcion de salida, calcula los tres itinerarios que tienen menor tiempo en el aire
    def miItinerario (aeropuerto1:String, aeropuerto2:String): List[Itinerario] = {
      val misItinerarios = funcionItinerario(aeropuerto1, aeropuerto2)

      if (misItinerarios.length <= 3) misItinerarios
      else {
        val tiempos =  for {
          i <- misItinerarios
          tiempoTotal = calcularTiempo(i)
        } yield (i, tiempoTotal)

        val salida = tiempos.sortBy(_._2)
        (((salida.unzip)._1).toList).take(3)
      }
    }

    miItinerario
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //guarda los valores de los itinerarios
    val obtenerItinerarios = itinerarios(vuelos, aeropuertos)

    //Funcion auxiliar
    def tresDeMenosEscalas(codigo1: String, codigo2: String): List[Itinerario] = {
      val todosItinerarios = obtenerItinerarios(codigo1, codigo2)
      // Ordenar los itinerarios por segun el numero de escalas y cambios de vuelo
      val itinerariosOrdenados = todosItinerarios.sortBy(itinerario => itinerario.map(_.Esc).sum + (itinerario.length - 1)).take(3)
      // Tomar los primeros tres itinerarios
      itinerariosOrdenados.take(3)
    }

    tresDeMenosEscalas
  }

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    //Busca el código en cada aeropuerto y cuendo lo encuentra devuelve su gmt, si no lo encuentra devuelve 0
    def gmtDeAeropuerto(codigo: String): Int = aeropuertos.find(_.Cod == codigo).map(_.GMT).getOrElse(0)

    //Define la hora exacta segun gmt de la cita en minutos, despues invoca a todos los vuelos
    //posibles y los filtra, si se llega más tarde que la cita el vuelo se considera no válido,
    //para todos los vuelos válidos escoge el que sale más tarde

    (cod1: String, cod2: String, h: Int, m: Int) => {
      val gmtDestino = gmtDeAeropuerto(cod2)
      val citaEnMinutos = minutosDesdeMedianoche(h, m, gmtDestino)
      val vuelosPosibles = itinerarios(vuelos,aeropuertos)(cod1,cod2)
      val vuelosValidos = vuelosPosibles.filter { listaVuelos =>
        if (listaVuelos.isEmpty) {
          false
        } else {
          val vuelo = listaVuelos.last
          val llegadaEnMinutos = minutosDesdeMedianoche(vuelo.HL, vuelo.ML, gmtDestino)
          llegadaEnMinutos <= citaEnMinutos
        }
      }

      /*A mi consideracion solo debe estar esta

      vuelosValidos.maxByOption { listaVuelos =>
        listaVuelos.headOption.map(vuelo => vuelo.HS * 60 + vuelo.MS).getOrElse(0)
      }.getOrElse(List.empty)

      Ya que cuando no se llega a tiempo simplemente deberia resultar en una lista vacía que
      significa que no hay vuelos que le sirvan al usuario
       */

      if (vuelosValidos==List()) { vuelosPosibles.maxByOption { listaVuelos =>
        listaVuelos.headOption.map(vuelo => vuelo.HS * 60 + vuelo.MS).getOrElse(0)
      }.getOrElse(List.empty)

      } else { vuelosValidos.maxByOption { listaVuelos =>
        listaVuelos.headOption.map(vuelo => vuelo.HS * 60 + vuelo.MS).getOrElse(0)
      }.getOrElse(List.empty)
      }

    }
  }

}
