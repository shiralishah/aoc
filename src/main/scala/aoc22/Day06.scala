package aoc22

object Day06A extends App {
  val input = scala.io.Source.fromResource("aoc22/day06.txt").mkString
  val windows = input.sliding(4).toSeq
  val indexOfUnique = windows.indexWhere(str => str.distinct.length == 4)
  println(indexOfUnique+4)
}

object Day06B extends App {
  val input = scala.io.Source.fromResource("aoc22/day06.txt").mkString
  val windows = input.sliding(14).toSeq
  val indexOfUnique = windows.indexWhere(str => str.distinct.length == 14)
  println(indexOfUnique+14)
}
