package aoc22

object Day01A extends App {
  val input = scala.io.Source.fromResource("aoc22/day01.txt").getLines().toSeq
  val (maxCal, _) = input.foldLeft((0L, 0L)) { case ((max, total), line) =>
    line match {
      case ""    => (if (total > max) total else max, 0L)
      case other => (max, total + other.toLong)
    }
  }
  println(maxCal)
}

object Day01B extends App {
  val input = scala.io.Source.fromResource("aoc22/day01.txt").getLines().toSeq
  val (parsedCals, lastTotal) = input.foldLeft((Seq.empty[Long], 0L)) { case ((calories, total), line) =>
    line match {
      case ""    => (calories :+ total, 0L)
      case other => (calories, total + other.toLong)
    }
  }
  val elfCals = (parsedCals :+ lastTotal).sortBy(-_)

  println(elfCals.take(3).sum)
}
