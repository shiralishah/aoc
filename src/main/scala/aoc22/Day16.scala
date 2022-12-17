package aoc22

case class Valve(label: String, rate: Int, neigbors: Set[String]) {
  def cost(minutes: Int): Int = rate * minutes
}

object Day16A extends App {
  val input = scala.io.Source.fromResource("aoc22/test2.txt").getLines().toSeq

  val startingValves = input.map {
    _ match {
      case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
        Valve(label, rate.toInt, neighbors.split(", ").toSet)
    }
  }
  val nonZero = startingValves.filterNot(_.rate == 0)

  val adj = startingValves.map(v => v -> v.neigbors.map(n => startingValves.find(_.label == n).get)).toMap

  val start = startingValves.find(_.label == "AA").get

  println(42)
  // dijkstra again
  // start at AA, get max spanning tree to all other valves from start
  // cost is remaining minutes*rate
  // choose max path from AA
  // run it again from new starting point (until time is out) - optimization until we've selected all nonzero valves
}
