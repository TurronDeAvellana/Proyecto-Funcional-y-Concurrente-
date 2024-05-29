import common._
import Datos._

//Se usa para la funcion secuencial de itinerarios llamada en funcion ItSalida
import Itinerarios.itinerarios

package object ItinerariosPar {
  def minutosDesdeMedianoche(hora: Int, minutos: Int, gmt: Int): Int = (60 * hora + minutos - gmt * 0.6).toInt

  //Version paralela de itinerariosAire
  def itinerariosAirePar(vuelos : List [ Vuelo ] , aeropuertos : List [ Aeropuerto] ) : ( String , String )=>List [Itinerario ]= {
    //Obtiene todos los itinerarios podibles
    val funcionItinerario = itinerarios(vuelos, aeropuertos)

    //Calcula el tiempo total de un solo vuelo
    def funTiempo(vuelo:Vuelo) = {

      val a1 = task(aeropuertos.filter(_.Cod == vuelo.Org))
      val a2 = task(aeropuertos.filter(_.Cod == vuelo.Dst))

      val gmtSalida = task((a1.join().head).GMT/100)
      val gmtLlegada = task((a2.join().head).GMT/100)

      val horaSalida = task((vuelo.HS + (vuelo.MS/60)) - gmtSalida.join())
      val horaLlegada = task((vuelo.HL + (vuelo.ML/60)) - gmtLlegada.join())
      val tiempoVuelo = horaLlegada.join() - horaSalida.join()

      if (tiempoVuelo <= 0) 24 + tiempoVuelo else tiempoVuelo
    }

    //Calcula el tiempo total de vuelo de un itinerario
    def calcularTiempo (v:Itinerario): Int= {
      val cadaTiempo =  v map (i => task(funTiempo(i)))
      (cadaTiempo map (l => l.join())).sum
    }

    //Funcion de salida, calcula los tres itinerarios que tienen menor tiempo en el aire
    def miItinerario (aeropuerto1:String, aeropuerto2:String): List[Itinerario] = {
      val misItinerarios = funcionItinerario(aeropuerto1, aeropuerto2)

      if (misItinerarios.length <= 3) misItinerarios
      else {
        val tiempos = misItinerarios map (c => (c, task(calcularTiempo(c))))
        val salida = tiempos.sortBy(_._2.join())
        (((salida.unzip)._1).toList).take(3)
      }
    }

    miItinerario
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