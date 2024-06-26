import Datos._

package object Itinerarios {

  def minutosDesdeMedianoche(hora: Int, minutos: Int, gmt: Int): Int = (60 * hora + minutos - gmt * 0.6).toInt //Realmente siempre es entero pero scala no sabe

  def comprobarValidez(itinerario: Itinerario, aeropuertos: List[Aeropuerto]): Boolean = {

    def gmtDeAeropuerto(codigo: String): Int = aeropuertos.find(_.Cod == codigo).map(_.GMT).getOrElse(0)

    itinerario.zip(itinerario.tail).forall { case (vueloActual, vueloSiguiente) =>
      val llegadaActual = minutosDesdeMedianoche(vueloActual.HL, vueloActual.ML, gmtDeAeropuerto(vueloActual.Dst))
      val salidaSiguiente = minutosDesdeMedianoche(vueloSiguiente.HS, vueloSiguiente.MS, gmtDeAeropuerto(vueloSiguiente.Org))
      llegadaActual < salidaSiguiente
    }
  }

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    //Funcion para encontrar los itinerarios, toma dos codigos, codigo1 el origen y codigo2 la llegada
    def encontrarItinerarios(codigo1: String, codigo2: String): List[Itinerario] = {


      // Función auxiliar para encontrar los vuelos que parten de un aeropuerto con el codigo dado
      def vuelosDesde(codigo: String): List[Vuelo] =
        vuelos.filter(v => v.Org == codigo)


      // Función auxiliar para encontrar los itinerarios de un vuelo hasta el destino de manera recursiva
      def itinerariosDesdeVuelo(vuelo: Vuelo, destino: String,aeropuertosVisitados: Set[String]): List[Itinerario] =
        if (vuelo.Dst == destino)
          //Si el aeropuerto de destino del vuelo coincide con el destino buscado, devuelve una lista con un solo
          // itinerario que contiene ese vuelo
          List(List(vuelo))
        else if (!aeropuertosVisitados.contains(vuelo.Dst))
          /* De lo contrario, busca todos los vuelos que parten desde el aeropuerto del destino actual
           * solamente cuando el set de aeropuertos visitados no contenga el destino del vuelo
           * y para cada uno de ellos calcula los itinerarios que parten desde ese vuelo hasta el destino buscado */
          vuelosDesde(vuelo.Dst).flatMap(v => itinerariosDesdeVuelo(v, destino,aeropuertosVisitados + vuelo.Dst).map(vuelo :: _))
        else List()
      // Con el flatMap concatenamos todos los itinerarios encotrados y map para agregar el vuelo actual al incicio
      //de cada uno de estos itinerarios


      /* lógica principal, obtiene todos los vuelos que parten desde el aeropuerto de origen y para cada uno de ellos, calcula
       * los itinerarios hasta el aeropuerto de destino, se hace una comprobacion de validez para asegurarse que
       * el vuelo en el destino no salga antes de que llegue el vuelo abordado */
      vuelosDesde(codigo1).flatMap(v => itinerariosDesdeVuelo(v, codigo2,Set(codigo1))).filter(itinerario => comprobarValidez(itinerario, aeropuertos))
    }

    encontrarItinerarios
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {


    // Calcula el tiempo total de viaje, incluyendo vuelos y tiempos de espera entre vuelos
    def calcularDuracion(itinerario: Itinerario): Int = {
      // Verifica si el itinerario esta vacio
      if (itinerario.isEmpty) return 0


      // Craemos un map para asociar código de aeropuerto con su diferencia de GMT.
      val aeropuertoMap = aeropuertos.map(a => a.Cod -> a.GMT).toMap


      // itera sobre pares de vuelos consecutivos en el itinerario, acumulando el tiempo total de viaje.
      // itinerario.zip(itinerario.tail) crea una lista de pares de vuelos consecutivos.
      itinerario.zip(itinerario.tail).foldLeft(0) { case (total, (vueloActual, vueloSiguiente)) =>
        //Obtener la diferencia GMT para los aeropuertos de origen y destino, si no hay retorna 0
        val gmtOrg = aeropuertoMap.getOrElse(vueloActual.Org, 0)
        val gmtDst = aeropuertoMap.getOrElse(vueloSiguiente.Org, 0)


        // Calcular los tiempos de salida y llegada en minutos, ajustados por GMT.
        val salidaActual = vueloActual.HS * 60 + vueloActual.MS + gmtOrg * 60
        val llegadaActual = vueloActual.HL * 60 + vueloActual.ML + gmtDst * 60
        val salidaSiguiente = vueloSiguiente.HS * 60 + vueloSiguiente.MS + gmtDst * 60


        //Calcular tiempo de vuelo y tiempo de espera
        val tiempoVuelo = llegadaActual - salidaActual


        // toma el mayor valor entre cero y la diferencia, esto para
        // asegurar que no sea negativp
        val tiempoEspera = Math.max(0, salidaSiguiente - llegadaActual)


        //Se añade el tiempo de vuelo y el tiempo de espera al total acumulado.
        total + tiempoVuelo + tiempoEspera
      } + {
        // Añadir el tiempo del último vuelo al total
        val ultimoVuelo = itinerario.last
        val gmtUltimoOrg = aeropuertoMap.getOrElse(ultimoVuelo.Org, 0)
        val gmtUltimoDst = aeropuertoMap.getOrElse(ultimoVuelo.Dst, 0)
        val salidaUltimo = ultimoVuelo.HS * 60 + ultimoVuelo.MS + gmtUltimoOrg * 60
        val llegadaUltimo = ultimoVuelo.HL * 60 + ultimoVuelo.ML + gmtUltimoDst * 60
        llegadaUltimo - salidaUltimo
      }
    }


    def itinerarios(codigo1: String, codigo2: String): List[Itinerario] = {
      def vuelosDesde(codigo: String): List[Vuelo] =
        vuelos.filter(v => v.Org == codigo)


      def itinerariosDesdeVuelo(vuelo: Vuelo, destino: String): List[Itinerario] =
        if (vuelo.Dst == destino) List(List(vuelo))
        else vuelosDesde(vuelo.Dst).flatMap(v => itinerariosDesdeVuelo(v, destino).map(vuelo :: _))


      vuelosDesde(codigo1).flatMap(v => itinerariosDesdeVuelo(v, codigo2))
    }


    (codigo1: String, codigo2: String) => itinerarios(codigo1, codigo2).sortBy(calcularDuracion).take(3)
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
      vuelosValidos.maxByOption { listaVuelos =>
        listaVuelos.headOption.map(vuelo => vuelo.HS * 60 + vuelo.MS).getOrElse(0)
      }.getOrElse(List.empty)
    }
  }

}
