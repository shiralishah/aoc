package aoc22

object Day04A extends App {
  val input = scala.io.Source.fromResource("aoc22/day04.txt").getLines().toSeq
  val overlap =
    input.foldLeft(0L) {
      case (count, pair) =>
        pair match {
          case s"$start1-$end1,$start2-$end2" =>
            val elf1 = (start1.toLong to end1.toLong)
            val elf2 = (start2.toLong to end2.toLong)
            if (elf1.forall(sec => elf2.contains(sec)) || elf2.forall(sec => elf1.contains(sec))) {
              count + 1
            } else {
              count
            }
        }
    }
    println(overlap)
}

object Day04B extends App {
  val input = scala.io.Source.fromResource("aoc22/day04.txt").getLines().toSeq
  val overlap =
    input.foldLeft(0L) {
      case (count, pair) =>
        pair match {
          case s"$start1-$end1,$start2-$end2" =>
            val elf1 = (start1.toLong to end1.toLong)
            val elf2 = (start2.toLong to end2.toLong)
            if (elf1.exists(sec => elf2.contains(sec)) || elf2.exists(sec => elf1.contains(sec))) {
              count + 1
            } else {
              count
            }
        }
    }
  println(overlap)
}
