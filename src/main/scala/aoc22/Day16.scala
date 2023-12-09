package aoc22

import scala.collection.mutable


case class Valve(label: String, rate: Int, neighbors: Seq[String]) {
  def pressure(minutes: Int): Int = rate * minutes
}

object Day16A extends App {
  val input = scala.io.Source.fromResource("aoc22/test.txt").getLines().toSeq

  val valveMap = input.map {
    case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
      (label -> Valve(label, rate.toInt, neighbors.split(", ").toSeq))
    case s"Valve $label has flow rate=$rate; tunnel leads to valve $neighbor" =>
      (label -> Valve(label, rate.toInt, Seq(neighbor)))
  }.toMap

  val distances = valveMap.keys.foldLeft(Map.empty[String, Seq[(String, Int)]]) {
    case (dist, name) =>
      val path = bfs(name).filter(x => filterBest(x._1))
      dist + (name -> path)
  }

  val allValued = valveMap.filter(x => filterBest(x._1) )

  def search(visited: Seq[String], curr: String, pressure: Long, time: Int) : Long = {
    if (visited.size >= allValued.size || time <= 0) {
      pressure
    } else {
      val vis = visited :+ curr
      val paths = distances(curr).filterNot(x => vis.contains(x._1)).map { n =>
        val t = time - n._2
        val press = pressure + valveMap(n._1).pressure(t)
        search(vis, n._1, press, t)
      }
      paths.max
    }

  }

  val finalPressure = search(Seq.empty, "AA", 0L, 30)

  println(finalPressure)

  def filterBest(str: String) =
    valveMap(str).rate > 0
  def bfs(start: String): Seq[(String, Int)] = {

    val queue = collection.mutable.Queue(start)
    val vist = collection.mutable.Map(start -> 1)

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      val valve = valveMap(current)
      valve.neighbors.filterNot(vist.contains).foreach { n =>
        queue.enqueue(n)
        vist(n) = vist(current) + 1
      }

    }

    vist.toSeq
  }

}

//def search (v: Seq[String], c: String, p: Long, t: Int): Seq[Long] = {
//  while (v.size < allValued.size && t > 0) {
//  val edges = distances (c).sortWith ((a, b) => a._2._2 > b._2._2)
//  val next = edges.filterNot (x => visited.contains (x._1) ).head
//  visited = visited :+ curr
//  time = time - next._2._1
//  pressure = pressure + valveMap (next._1).pressure (time)
//  curr = next._1
//  }
//  }

//object Day16A extends App {
//  val input = scala.io.Source.fromResource("aoc22/run.txt").getLines().toSeq
//
//  val valveMap = input.map {
//    case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
//      (label -> Valve(label, rate.toInt, neighbors.split(", ").toSeq))
//    case s"Valve $label has flow rate=$rate; tunnel leads to valve $neighbor" =>
//      (label -> Valve(label, rate.toInt, Seq(neighbor)))
//  }.toMap
//
//  val distances = valveMap.keys.foldLeft(Map.empty[String, Map[String, (Int, Float)]]) {
//    case (dist, name) =>
//      val path = bfs(name).filter(x => valveMap(x._1).rate > 0)
//      dist + (name -> path)
//  }
//
//  def bfs(start: String): Map[String, (Int, Float)] = {
//
//    val queue = collection.mutable.Queue(start)
//    val visited = collection.mutable.Map(start -> (0, 0F))
//
//    while (queue.nonEmpty) {
//      val current = queue.dequeue()
//      val valve = valveMap(current)
//      valve.neighbors.filterNot(visited.contains).foreach { n =>
//        queue.enqueue(n)
//        val len = visited(current)._1 + 1
//        visited(n) = (len, valveMap(n).rate/(len.toFloat))
//      }
//
//    }
//    visited.toMap
//  }
//
//
//}


object Day16B extends App {
  val input = scala.io.Source.fromResource("aoc22/test.txt").getLines().toSeq

  val valveMap = input.map {
    case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
      (label -> Valve(label, rate.toInt, neighbors.split(", ").toSeq))
    case s"Valve $label has flow rate=$rate; tunnel leads to valve $neighbor" =>
      (label -> Valve(label, rate.toInt, Seq(neighbor)))
  }.toMap

  val positiveValves = valveMap.collect {
    case (k, v) if v.rate > 0 => k
  }.toSet
  val distances = valveMap.keys.map(k => (k -> findPaths(valveMap, k))).toMap

  val finalPaths = search("AA", 26)
  val bothMax = (for {
    (a, b) <- finalPaths
    (x, y) <- finalPaths
  } yield {
    if (a.intersect(x).isEmpty) b + y else 0
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

  def search(start: String, initial: Int): Map[Set[String], Int] = {
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
