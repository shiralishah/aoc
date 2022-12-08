package aoc21

object Day11A extends App {
  val input = scala.io.Source.fromResource("aoc21/day10.txt").getLines().toSeq
  val octopus = (for {
    (row, y) <- input.zipWithIndex
    (v, x)   <- row.zipWithIndex
  } yield Point(x, y) -> v.asDigit).toMap

}
