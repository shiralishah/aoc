package aoc21

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class VolcanoMap(grid: Array[Array[Int]]) {
  val height = grid.length
  val width  = grid.head.length

  def apply(p: Point): Int = grid(p.y)(p.x)

  def getNeighbors(p: Point): Seq[Point] =
    Seq(
      Option.when(p.x > 0)(Point(p.x - 1, p.y)),
      Option.when(p.x < width - 1)(Point(p.x + 1, p.y)),
      Option.when(p.y > 0)(Point(p.x, p.y - 1)),
      Option.when(p.y < height - 1)(Point(p.x, p.y + 1))
    ).flatMap(Seq.from)

  def getSinkSize(p: Point): Int =
    explore(Set.empty[Point], Queue(p), Set(p))

  @tailrec
  private def explore(visited: Set[Point], queue: Queue[Point], basin: Set[Point]): Int =
    if (queue.isEmpty) {
      basin.size
    } else {
      val (p, left) = queue.dequeue
      val neighbors = getNeighbors(p).collect {
        case p if !visited.contains(p) && apply(p) != 9 => p
      }
      explore(visited + p, left ++ neighbors, basin ++ neighbors)
    }
}

object Day09A extends App {
  val input   = scala.io.Source.fromResource("aoc21/day09.txt").getLines().map(_.toCharArray.map(_.asDigit)).toArray
  val heights = VolcanoMap(input)
  val priorities =
    (0 until heights.height).flatMap { j =>
      (0 until heights.width).map { i =>
        val p = Point(i, j)
        (heights(p), heights.getNeighbors(p).map(heights.apply))
      }.collect {
        case (smoke, neighbors) if neighbors.forall(_ > smoke) => smoke + 1
      }
    }.sum

  println(priorities)
}

object Day09B extends App {
  val input   = scala.io.Source.fromResource("aoc21/day09.txt").getLines().map(_.toCharArray.map(_.asDigit)).toArray
  val heights = VolcanoMap(input)
  val sinks =
    (0 until heights.height).flatMap { j =>
      (0 until heights.width).map { i =>
        val p = Point(i, j)
        (p, heights(p), heights.getNeighbors(p))
      }.collect {
        case (point, smoke, neighbors) if neighbors.map(heights.apply).forall(_ > smoke) => point
      }
    }
  val basins = sinks.map(heights.getSinkSize).sorted.takeRight(3)
  val mult   = basins(0) * basins(1) * basins(2)
  println(mult)
}
