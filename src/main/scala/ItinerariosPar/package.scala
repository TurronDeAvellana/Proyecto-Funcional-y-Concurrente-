import scala.collection.parallel.immutable._
import common._

package object ItinerariosPar {

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

}
