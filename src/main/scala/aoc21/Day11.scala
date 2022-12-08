package aoc21

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Counter {
  def finished: Boolean

  def flashes: Int

  def stepNum: Int

  def increment: Counter

  def addFlashes(n: Int): Counter
}

case class LimitCounter(flashes: Int, stepNum: Int, end: Int) extends Counter {
  override def finished: Boolean = stepNum == end

  override def addFlashes(n: Int): Counter = this.copy(flashes = flashes + n)

  override def increment: Counter = this.copy(stepNum = stepNum + 1)
}

case class AllCounter(flashes: Int, stepNum: Int, changeNum: Int, previous: Int) extends Counter {
  override def finished: Boolean = changeNum == (flashes - previous)

  override def addFlashes(n: Int): Counter = this.copy(flashes = flashes + n, previous = flashes)

  override def increment: Counter = this.copy(stepNum = stepNum + 1)
}

case class OctopusGrid(inputMap: Map[Point, Int]) {

  def runStep(counter: Counter): Counter = runStep(counter, inputMap)

  @tailrec
  private def runStep(counter: Counter, sit: Map[Point, Int]): Counter =
    if (counter.finished) counter
    else {
      val inc = sit.map { case (p, v) =>
        (p, v + 1)
      }
      val flashing = inc.collect {
        case (p, v) if v > 9 => p
      }
      val spread = spreadFlash(Queue(flashing).flatten, Set.empty, inc)
      val flashNum = spread.collect {
        case (_, v) if v > 9 => 1
      }.sum
      val reset = spread.map {
        case (p, v) if v > 9 => (p, 0)
        case a               => a
      }
      runStep(counter.addFlashes(flashNum).increment, reset)
    }

  @tailrec
  private def spreadFlash(
    queue: Queue[Point],
    flashed: Set[Point],
    sit: Map[Point, Int]
  ): Map[Point, Int] =
    queue.dequeueOption match {
      case None => sit
      case Some((p, rem)) =>
        sit.get(p) match {
          case Some(v) if v > 9 && !flashed(p) =>
            val adjacent = Seq(
              p.copy(x = p.x - 1),
              p.copy(x = p.x + 1),
              p.copy(y = p.y - 1),
              p.copy(y = p.y + 1),
              p.copy(x = p.x - 1, y = p.y - 1),
              p.copy(x = p.x - 1, y = p.y + 1),
              p.copy(x = p.x + 1, y = p.y - 1),
              p.copy(x = p.x + 1, y = p.y + 1)
            )
            val nextSit = adjacent.foldLeft(sit) { case (grid, point) =>
              grid.get(point) match {
                case Some(value) => grid.updated(point, value + 1)
                case _           => grid
              }
            }
            spreadFlash(rem.appendedAll(adjacent), flashed + p, nextSit)
          case _ => spreadFlash(rem, flashed, sit)
        }
    }
}

object Day11A extends App {
  val input = scala.io.Source.fromResource("aoc21/day11.txt").getLines().toSeq
  val octopei = OctopusGrid((for {
    (row, y) <- input.zipWithIndex
    (v, x)   <- row.zipWithIndex
  } yield Point(x, y) -> v.asDigit).toMap)

  val end = octopei.runStep(LimitCounter(0, 0, 100))
  println(end.flashes)

}

object Day11B extends App {
  val input = scala.io.Source.fromResource("aoc21/day11.txt").getLines().toSeq
  val octopei = OctopusGrid((for {
    (row, y) <- input.zipWithIndex
    (v, x)   <- row.zipWithIndex
  } yield Point(x, y) -> v.asDigit).toMap)

  val end = octopei.runStep(AllCounter(0, 0, octopei.inputMap.size, 0))
  println(end.stepNum)

}
