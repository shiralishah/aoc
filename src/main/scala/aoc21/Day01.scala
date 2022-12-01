package aoc21

object Day01A extends App {
  val input = scala.io.Source.fromResource("aoc21/day01.txt").getLines().toSeq
  val (count, _) = input.foldLeft((-1, 0)) {case ((incCount, prevDepth), depth) =>
    val d = depth.toInt
    (if (d > prevDepth) incCount+1 else incCount, d)
  }

  println(count)
}

object Day01B extends App {
  val input = scala.io.Source.fromResource("aoc21/day01.txt").getLines().toSeq
  val groupedReadings = input.map(_.toInt).sliding(3).map(_.sum)
  val (count, _) = groupedReadings.foldLeft((-1, 0)) {case ((incCount, prevDepth), depth) =>
    (if (depth > prevDepth) incCount+1 else incCount, depth)
  }

  println(count)
}
