package aoc22

import scala.collection.immutable.Queue

case class HillPoint(x: Int, y: Int, v: Char) {
  def getHeight: Char =
    if (v == 'S') 'a'
    else if (v == 'E') 'z'
    else v
}

object Day12A extends App {
  val input           = scala.io.Source.fromResource("aoc22/day12.txt").getLines().map(_.toCharArray).toArray
  val (height, width) = (input.length, input.head.length)

  val array: Array[Array[HillPoint]] = input.zipWithIndex.map { case (row, y) =>
    row.zipWithIndex.map { case (c, x) =>
      HillPoint(x, y, c)
    }
  }

  val start = array.flatten.find(_.v == 'S').get
  val end   = array.flatten.find(_.v == 'E').get

  val adj = buildAdj(array)
  val costMap: Map[HillPoint, Int] = adj.foldLeft(Map.empty[HillPoint, Int]) { case (map, (p, _)) =>
    map.updated(p, Int.MaxValue)
  }

  val finalCosts = dijkstra(Queue((end, 0)), costMap.updated(end, 0))
  val shortest   = finalCosts.get(start).get
  println(shortest)

  def dijkstra(queue: Queue[(HillPoint, Int)], cost: Map[HillPoint, Int]): Map[HillPoint, Int] =
    queue.dequeueOption match {
      case None => cost
      case Some(((point, c), que)) =>
        if (cost.get(point).get != c) {
          dijkstra(queue, cost)
        } else {
          val neighbors = adj.getOrElse(point, Seq.empty)
          val (updatedCost, updatedQueue) =
            neighbors.foldLeft(cost, que) { case ((map, q), neighbor) =>
              if (map.get(neighbor).get > (c + 1)) {
                (map.updated(neighbor, c + 1), q :+ (neighbor, c + 1))
              } else (map, q)
            }
          dijkstra(updatedQueue, updatedCost)
        }
    }

  def buildAdj(array: Array[Array[HillPoint]]): Map[HillPoint, Seq[HillPoint]] = {
    def hasEdge(a: HillPoint, b: HillPoint): Boolean =
      b.getHeight >= (a.getHeight - 1)

    array.flatten.foldLeft(Map.empty[HillPoint, Seq[HillPoint]]) { case (adjMat, p) =>
      val potentialNeighbors = Seq(
        Option.when(p.x < width - 1)(array(p.y)(p.x + 1)),
        Option.when(p.x > 0)(array(p.y)(p.x - 1)),
        Option.when(p.y < height - 1)(array(p.y + 1)(p.x)),
        Option.when(p.y > 0)(array(p.y - 1)(p.x))
      ).flatten
      val neighbors = potentialNeighbors.filter(n => hasEdge(p, n))
      Option
        .when(neighbors.nonEmpty)(neighbors)
        .map(neighs => adjMat.updated(p, neighs))
        .getOrElse(adjMat)
    }
  }
}

object Day12B extends App {
  val input           = scala.io.Source.fromResource("aoc22/day12.txt").getLines().map(_.toCharArray).toArray
  val (height, width) = (input.length, input.head.length)

  val array: Array[Array[HillPoint]] = input.zipWithIndex.map { case (row, y) =>
    row.zipWithIndex.map { case (c, x) =>
      HillPoint(x, y, c)
    }
  }

  val start = array.flatten.find(_.v == 'S').get
  val end   = array.flatten.find(_.v == 'E').get

  val adj = buildAdj(array)
  val costMap: Map[HillPoint, Int] = adj.foldLeft(Map.empty[HillPoint, Int]) { case (map, (p, _)) =>
    map.updated(p, Int.MaxValue)
  }

  val finalCosts = dijkstra(Queue((end, 0)), costMap.updated(end, 0))
  val shortest   = finalCosts.filter(_._1.getHeight == 'a').minBy(_._2)._2
  println(shortest)

  def dijkstra(queue: Queue[(HillPoint, Int)], cost: Map[HillPoint, Int]): Map[HillPoint, Int] =
    queue.dequeueOption match {
      case None => cost
      case Some(((point, c), que)) =>
        if (cost.get(point).get != c) {
          dijkstra(queue, cost)
        } else {
          val neighbors = adj.getOrElse(point, Seq.empty)
          val (updatedCost, updatedQueue) =
            neighbors.foldLeft(cost, que) { case ((map, q), neighbor) =>
              if (map.get(neighbor).get > (c + 1)) {
                (map.updated(neighbor, c + 1), q :+ (neighbor, c + 1))
              } else (map, q)
            }
          dijkstra(updatedQueue, updatedCost)
        }
    }

  def buildAdj(array: Array[Array[HillPoint]]): Map[HillPoint, Seq[HillPoint]] = {
    def hasEdge(a: HillPoint, b: HillPoint): Boolean =
      b.getHeight >= (a.getHeight - 1)

    array.flatten.foldLeft(Map.empty[HillPoint, Seq[HillPoint]]) { case (adjMat, p) =>
      val potentialNeighbors = Seq(
        Option.when(p.x < width - 1)(array(p.y)(p.x + 1)),
        Option.when(p.x > 0)(array(p.y)(p.x - 1)),
        Option.when(p.y < height - 1)(array(p.y + 1)(p.x)),
        Option.when(p.y > 0)(array(p.y - 1)(p.x))
      ).flatten
      val neighbors = potentialNeighbors.filter(n => hasEdge(p, n))
      Option
        .when(neighbors.nonEmpty)(neighbors)
        .map(neighs => adjMat.updated(p, neighs))
        .getOrElse(adjMat)
    }
  }
}
