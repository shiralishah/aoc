package aoc22

import scala.collection.mutable


case class Valve(label: String, rate: Int, neighbors: Seq[String]) {
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

  val distances = (valveMap.keys.filter(filterBest).toSeq :+ "AA")
    .foldLeft(Map.empty[String, Seq[(String, Int)]]) {
    case (dist, name) =>
      val path = bfs(name).filter(x => filterBest(x._1))
      dist + (name -> path)
  }

  var memory: mutable.Map[(Int, String, String), Long] = mutable.Map.empty

  val finalPressure = dfs(30, "AA", Set.empty)

  println(finalPressure)

  def filterBest(str: String) =
    valveMap(str).rate > 0

  def dfs(time: Int, curr: String, open: Set[String]): Long = {
    val openStr = open.mkString(",")
    if (memory.contains((time, curr, openStr))) {
      memory((time, curr, openStr))
    } else {
      val maximum = distances(curr).foldLeft(0L) {
        case (maxVal, (neighbor, cost)) =>
          val remTime = time - cost
          if (remTime <= 0 || open.contains(neighbor)) {
            maxVal
          } else {
            Math.max(maxVal, dfs(remTime, neighbor, open + neighbor) + valveMap(neighbor).pressure(remTime))
          }
      }
      memory += ((time, curr, openStr) -> maximum)
      maximum
    }
  }

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

object Day16B extends App {
  val input = scala.io.Source.fromResource("aoc22/rungit .txt").getLines().toSeq

  val valveMap = input.map {
    case s"Valve $label has flow rate=$rate; tunnels lead to valves $neighbors" =>
      (label -> Valve(label, rate.toInt, neighbors.split(", ").toSeq))
    case s"Valve $label has flow rate=$rate; tunnel leads to valve $neighbor" =>
      (label -> Valve(label, rate.toInt, Seq(neighbor)))
  }.toMap

  val valuableValves = valveMap.keys.filter(x => valveMap(x).rate > 0).toSet

  val distances = ( valuableValves + "AA")
    .foldLeft(Map.empty[String, Seq[(String, Int)]]) {
      case (dist, name) =>
        val path = bfs(name).filter(x => valuableValves.contains(x._1))
        dist + (name -> path)
    }

  var memory: mutable.Map[(Int, String, String), Long] = mutable.Map.empty

  val powerSet = valuableValves.foldLeft(Set(Set.empty[String])) {
    (acc, element) =>
      acc ++ acc.map(subset => subset + element)
  }

  val (maxPressure, _) = powerSet.foldLeft(0L, Set.empty[Set[String]]){
    case ((maxVal, checked), opened) =>
      val elephant: Set[String] = valuableValves.diff(opened)
      if (checked.contains(elephant)) {
        (maxVal, checked)
      } else {
        val sum = dfs(26, "AA", opened) +
          dfs(26, "AA", elephant)
        (Math.max(maxVal, sum), checked + opened)
      }
  }

  println(maxPressure)

  def dfs(time: Int, curr: String, open: Set[String]): Long = {
    val openStr = open.mkString(",")
    if (memory.contains((time, curr, openStr))) {
      memory((time, curr, openStr))
    } else {
      val maximum = distances(curr).foldLeft(0L) {
        case (maxVal, (neighbor, cost)) =>
          val remTime = time - cost
          if (remTime <= 0 || open.contains(neighbor)) {
            maxVal
          } else {
            Math.max(maxVal, dfs(remTime, neighbor, open + neighbor) + valveMap(neighbor).pressure(remTime))
          }
      }
      memory += ((time, curr, openStr) -> maximum)
      maximum
    }
  }

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