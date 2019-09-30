import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class prova extends AggregateProgram with FieldUtils with StandardSensors with ScafiAlchemistSupport {

  // Conta i round che passano
  def count() : Int = {
    rep(0) { n => n+1 }
  }

  // Vale sempre il primo valore mai assunto dall'argomento.
  def constant[T](value: => T): T = {
    rep(value) { old => old }
  }

  // Calcolo di distanze approssimato (gradiente).
  // isSource: se il device corrente e' una sorgente
  // metric:   funzione che calcola la distanza dispetto ai vicini del device corrente
  // returns:  la lunghezza del cammino minimo nella rete verso una sorgente
  def gradient(isSource: Boolean, metric: => Double): Double = {
    rep(Double.PositiveInfinity) { distance =>
      mux(isSource){ 0.0 }{ minHood{ nbr{distance} + metric } }
    }
  }

  /*
  4. muoversi verso il vicino che ha meno vicini
  5. muoversi lontano dal vicino che ha più vicini
  6. combinare gli ultimi due punti: ogni device è attratto dal vicino con meno vicini, e respinto dal vicino con più vicini.
  */

  /*Per muovere un device, bisogna memorizzare una posizione List(x,y) nella molecola target: node.put("target", ...);
  Sostituite la porzione di codice che ora si occupa del movimento con quanto richiesto.
  Si può ottenere la posizione corrente di un device tramite il metodo getCoordinates().*/

  /*Nei primi esercizi, ragionate dove usare nbr ("guardo i vicini") e rep ("guardo il passato").*/

  // rep: supports dynamically evolving fields by having an expression e depend on its previous value w (with x being the initial state).
  // rep x w e
  def evaluateMaxOfDevice(numberOfNeighbors: Int) = {
    rep(Int.MinValue) { maxNumberOfNeighbors => max(maxNumberOfNeighbors, numberOfNeighbors) }
  }

  //  nbr: gathers a map at each device (actually, a field) from all neighbors
  // (including itself) to their latest value of s. Built-in“hood” functions then
  // summarize such maps, e.g., min-hood(m) finds the minimum value in map m
  // (excluding the value associated to the device itself)
  def evaluateMaxOfNetwork(maxNumberOfNeighbors: Int) = {
    rep(Int.MinValue) { maxNum =>
      maxHood{ nbr{max(maxNum, maxNumberOfNeighbors)} }
    }
  }

  override def main(): Any = {
    node.put("language", "scafi")
    val actualNode: Node[Any] = alchemistEnvironment.getNodeByID(mid)
    // la sorgente e' il nodo 0 (fermo nel mezzo)
    val sourceID = 0
    val isSource = mid == sourceID
    // salvo la stima di distanza e quella esatta
    node.put("distance", alchemistEnvironment.getDistanceBetweenNodes(actualNode, alchemistEnvironment.getNodeByID(sourceID))) // esatta
    node.put("gradient", gradient(isSource, nbrRange)) // stima

    // determino un tempo in cui iniziare a muovermi a caso tra 0 e 200
    val timeToGo = constant(200 * nextRandom())
    node.put("timeToGo", timeToGo)

    // NUMERO DI DEVICE VICINI
    var numberOfNeighbors = alchemistEnvironment.getNeighborhood(actualNode).size()
    node.put("numberOfNeighbors", numberOfNeighbors)

    // MASSIMO NUMERO DI DEVICE VICINI PER IL NODO ATTUALE
    node.put("maxNumberOfNeighbors", evaluateMaxOfDevice(numberOfNeighbors))

    // MASSIMO NUMERO DI DEVICE VICINI DELLA RETE
    node.put("maxNumberOfNeighborsInNetwork", evaluateMaxOfNetwork(numberOfNeighbors))

    // VICINO CON MENO VICINI
    /*val neighbors = getNodeNeighbors(actualNode)

    def minNeighbors(e1: Node[Any], e2: Node[Any]): Node[Any] = if (getNodeNeighbors(e1).length < getNodeNeighbors(e2).length) e1 else e2
    def maxNeighbors(e1: Node[Any], e2: Node[Any]): Node[Any] = if (getNodeNeighbors(e1).length > getNodeNeighbors(e2).length) e1 else e2

    val minNbr = neighbors.reduceLeft(minNeighbors)
    val maxNbr = neighbors.reduceLeft(maxNeighbors)*/

    // determino un luogo in cui muovermi, se e' il momento di farlo
    branch(timestamp() < timeToGo){
      // non e' ancora ora, e non faccio nulla
    } {
      // SPOSTAMENTO
      val target = constant(List(8*nextRandom()-4, 4*nextRandom()-2))
      node.put("target", target)
    }

    0
  }

  /*def getNodeNeighbors(actualNode: Node[Any])={
    alchemistEnvironment.getNeighborhood(actualNode).getNeighbors.toArray(Node[Any])
  }*/

  // Crea una coppia
  def pair[A,B](x : A, y : B) : Tuple2[A,B] = {
    Tuple2(x,y)
  }

  // Primo elemento di una coppia
  def fst[A,B](t : Tuple2[A,B]) : A = {
    t._1
  }

  // Secondo elemento di una coppia
  def snd[A,B](t : Tuple2[A,B]) : B = {
    t._2
  }

  // Massimo tra due elementi
  def max[T](x : T, y : T)(implicit ord: Ordering[T]) : T = {
    ord.max(x, y)
  }

  // Somma di un campo di numeri
  def sumHood[T](x : => T)(implicit numEv: Numeric[T]) : T = {
    includingSelf.sumHood(x)
  }

  // Coordinate correnti come lista di due Double
  def getCoordinates(): List[Double] = {
    alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(mid)).getCartesianCoordinates.toList
  }
}
