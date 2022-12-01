package aoc22

object Day01 extends App {
  val input = scala.io.Source.fromResource("aoc22/day01.txt").getLines().toSeq
  val (parsedCals, lastTotal) = input.foldLeft((Seq.empty[Long], 0L)){ case((calories, total), line) =>
    line match {
      case "" => (calories :+ total, 0L)
      case other => (calories, total + other.toLong)
    }
  }
  val elfCals = (parsedCals :+ lastTotal).sortBy(-_)

  println(elfCals.head)
  println(elfCals.take(3).sum)
}