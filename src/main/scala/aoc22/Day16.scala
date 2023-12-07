package aoc22


case class Valve(label: String, rate: Int, neighbors: Seq[String], isOpen: Boolean = false) {
  def pressure(minutes: Int): Int = rate * minutes
}

object Day16A extends App {
  val input = scala.io.Source.fromResource("aoc22/run.txt").getLines().toSeq

  val valveMap = input.map {
    case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
      (label -> Valve(label, rate.toInt, neighbors.split(", ").toSeq))
    case s"Valve $label has flow rate=$rate; tunnel leads to valve $neighbor" =>
      (label -> Valve(label, rate.toInt, Seq(neighbor)))
  }.toMap

  val positiveValves = valveMap.collect{
    case(k, v) if v.rate > 0 => k
  }.toSet
  val distances = valveMap.keys.map(k => (k -> findPaths(valveMap, k))).toMap

  val finalPaths = search("AA", 30)
  val maxValue = finalPaths.values.max
  println(maxValue)


  def findPaths(valves: Map[String, Valve], start: String): Map[String, Int] = {

    val queue = collection.mutable.Queue(start)
    val dist = collection.mutable.Map(start -> 1)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      valves(current).neighbors.filterNot(dist.contains).foreach { n =>
        queue.enqueue(n)
        dist(n) = dist(current) + 1
      }
    }
    dist.toMap
  }

  def search(start: String, initial: Int): Map[Set[String], Int]= {
    val values = collection.mutable.Map[Set[String], Int]().withDefaultValue(0)

    def move(queue: Set[String], completed: Set[String], from: String, time: Int, pressure: Int): Unit = {
      values(completed) = values(completed).max(pressure)

      for (n <- queue) {
        val rem = time - distances(from)(n)
        val press = if (rem > 0) {
          valveMap(n).pressure(rem)
        } else 0
        move(queue - n, completed + n, n, rem, pressure + press)
      }
    }

    move(positiveValves, Set(), start, initial, 0)
    values.toMap
  }


}


object Day16B extends App {
  val input = scala.io.Source.fromResource("aoc22/test.txt").getLines().toSeq

  val valveMap = input.map {
    case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
      (label -> Valve(label, rate.toInt, neighbors.split(", ").toSeq))
    case s"Valve $label has flow rate=$rate; tunnel leads to valve $neighbor" =>
      (label -> Valve(label, rate.toInt, Seq(neighbor)))
  }.toMap

  val positiveValves = valveMap.collect{
    case(k, v) if v.rate > 0 => k
  }.toSet
  val distances = valveMap.keys.map(k => (k -> findPaths(valveMap, k))).toMap

  val finalPaths = search("AA", 26)
  val bothMax = (for {
    (a, b) <- finalPaths
    (x, y) <- finalPaths
  } yield {
    if (a.intersect(x).isEmpty) b+y else 0
  }).max

  println(bothMax)


  def findPaths(valves: Map[String, Valve], start: String): Map[String, Int] = {

    val queue = collection.mutable.Queue(start)
    val dist = collection.mutable.Map(start -> 1)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      valves(current).neighbors.filterNot(dist.contains).foreach { n =>
        queue.enqueue(n)
        dist(n) = dist(current) + 1
      }
    }
    dist.toMap
  }

  def search(start: String, initial: Int): Map[Set[String], Int]= {
    val values = collection.mutable.Map[Set[String], Int]().withDefaultValue(0)

    def move(queue: Set[String], completed: Set[String], from: String, time: Int, pressure: Int): Unit = {
      values(completed) = values(completed).max(pressure)

      for (n <- queue) {
        val rem = time - distances(from)(n)
        val press = if (rem > 0) {
          valveMap(n).pressure(rem)
        } else 0
        move(queue - n, completed + n, n, rem, pressure + press)
      }
    }

    move(positiveValves, Set(), start, initial, 0)
    values.toMap
  }


}
